package ru.yole.jitwatch.languages

import com.intellij.openapi.application.*
import com.intellij.openapi.project.*
import com.intellij.openapi.util.*
import com.intellij.psi.*
import com.intellij.psi.util.*
import org.adoptopenjdk.jitwatch.model.*
import org.jetbrains.kotlin.codegen.*
import org.jetbrains.kotlin.codegen.state.*
import org.jetbrains.kotlin.config.*
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.idea.caches.resolve.*
import org.jetbrains.kotlin.idea.search.*
import org.jetbrains.kotlin.idea.stubindex.*
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.*
import org.jetbrains.kotlin.resolve.*
import org.jetbrains.kotlin.resolve.calls.callUtil.*
import org.jetbrains.kotlin.resolve.descriptorUtil.*
import org.jetbrains.kotlin.resolve.lazy.*

class JitWatchKotlinSupport : JitWatchLanguageSupport<KtClassOrObject, KtCallableDeclaration> {
    override fun getAllClasses(file: PsiFile): List<KtClassOrObject> =
        ApplicationManager.getApplication().runReadAction(Computable {
            PsiTreeUtil.collectElementsOfType(file, KtClassOrObject::class.java).toList().also {
                println("CLASSES[${file.name}]: ${it.joinToString { it.name.toString() }}")
            }
        })

    override fun findClass(project: Project, metaClass: MetaClass): KtClassOrObject? =
        KotlinFullClassNameIndex.getInstance().get(metaClass.fullyQualifiedName, project, project.allScope())
            .also {
                println("FIND[${metaClass.fullyQualifiedName}]: ${it.joinToString { it.name.toString() }}")
            }.firstOrNull()

    override fun getAllMethods(cls: KtClassOrObject): List<KtCallableDeclaration> =
        cls.declarations.filterIsInstance<KtCallableDeclaration>()

    override fun isMethod(element: PsiElement) = element is KtFunction && element !is KtFunctionLiteral

    override fun findMethodAtOffset(file: PsiFile, offset: Int): KtFunction? =
        PsiTreeUtil.getParentOfType(file.findElementAt(offset), KtFunction::class.java)

    override fun getNameRange(element: PsiElement): TextRange = when (element) {
        is PsiNameIdentifierOwner -> element.nameIdentifier?.textRange ?: element.textRange
        else -> element.textRange
    }

    override fun getClassVMName(cls: KtClassOrObject): String? {
        val descriptor = cls.resolveToDescriptor() as? ClassDescriptor ?: return null
        return getClassDescriptorVMName(descriptor)
    }

    private fun getClassDescriptorVMName(descriptor: ClassifierDescriptorWithTypeParameters): String {
        val typeMapper = KotlinTypeMapper(
            BindingContext.EMPTY,
            ClassBuilderMode.LIGHT_CLASSES,
            getModuleName(descriptor),
            KotlinTypeMapper.LANGUAGE_VERSION_SETTINGS_DEFAULT,
            IncompatibleClassTracker.DoNothing, JvmTarget.JVM_1_8
        )

        return descriptor.fqNameSafe.asString()
    }

    // FIXME descriptor.module.stableName is '<module-name>' instead of 'module-name' - Is it a good idea to simply try remove <>?
    private fun getModuleName(descriptor: DeclarationDescriptor) =
        descriptor.module.stableName?.toString()?.removeSurrounding("<", ">") ?: "<unknwon>"

    override fun getContainingClass(method: KtCallableDeclaration): KtClassOrObject? = method.containingClassOrObject

    override fun matchesSignature(
        method: KtCallableDeclaration,
        memberName: String,
        paramTypeNames: List<String>,
        returnTypeName: String
    ): Boolean {
        val descriptor = method.resolveToDescriptor() as? FunctionDescriptor ?: return false
        return matchesSignature(descriptor, memberName, paramTypeNames, returnTypeName)
    }

    private fun matchesSignature(
        descriptor: FunctionDescriptor,
        memberName: String,
        paramTypeNames: List<String>,
        returnTypeName: String
    ): Boolean {

        val typeMapper = KotlinTypeMapper(
            BindingContext.EMPTY,
            ClassBuilderMode.LIGHT_CLASSES,
            getModuleName(descriptor),
            KotlinTypeMapper.LANGUAGE_VERSION_SETTINGS_DEFAULT,
            // FIXME Figure out how to properly choose JvmTarget
            IncompatibleClassTracker.DoNothing,
            JvmTarget.JVM_1_8,
            false // FIXME what significance does this have?
        )

        val signature = typeMapper.mapAsmMethod(descriptor)

        val expectedName = if (descriptor is ConstructorDescriptor)
            DescriptorUtils.getFqNameFromTopLevelClass(descriptor.containingDeclaration).asString()
        else
            signature.name

        if (expectedName != memberName) return false
        if (signature.argumentTypes.size != paramTypeNames.size) return false
        val paramTypes = paramTypeNames zip signature.argumentTypes.map { it.className }
        if (paramTypes.any { it.first != it.second })
            return false
        if (returnTypeName != signature.returnType.className) {
            return false
        }
        return true
    }

    override fun findCallToMember(
        file: PsiFile,
        offset: Int,
        calleeMember: MemberSignatureParts,
        sameLineCallIndex: Int
    ): PsiElement? {
        var result: PsiElement? = null
        var curIndex = 0
        processCalls(file, offset) { expression, resultingDescriptor ->
            val target = resultingDescriptor as? FunctionDescriptor ?: return@processCalls
            if (matchesSignature(target, calleeMember.memberName, calleeMember.paramTypes, calleeMember.returnType)) {
                if (curIndex == sameLineCallIndex) {
                    result = expression.calleeExpression
                }
                curIndex++
            }

        }
        return result
    }

    override fun findAllocation(file: PsiFile, offset: Int, jvmName: String): PsiElement? {
        var result: PsiElement? = null
        processCalls(file, offset) { expression, resultingDescriptor ->
            val target = resultingDescriptor as? ConstructorDescriptor ?: return@processCalls
            val createdClass = getClassDescriptorVMName(target.containingDeclaration)
            if (createdClass == jvmName) {
                result = expression.calleeExpression
            }
        }
        return result
    }

    private fun processCalls(file: PsiFile, offset: Int, callback: (KtCallExpression, CallableDescriptor) -> Unit) {
        val expr = file.findLargestExpressionAt(offset) ?: return
        expr.accept(object : KtTreeVisitorVoid() {
            override fun visitCallExpression(expression: KtCallExpression) {
                super.visitCallExpression(expression)
                val bindingContext = expression.analyze(BodyResolveMode.PARTIAL)
                val resolvedCall = expression.getResolvedCall(bindingContext) ?: return

                callback(expression, resolvedCall.resultingDescriptor)
            }
        })
    }
}

private fun PsiFile.findLargestExpressionAt(offset: Int): KtExpression? {
    var element = PsiTreeUtil.getParentOfType(findElementAt(offset), KtExpression::class.java) ?: return null
    while (true) {
        val parent = PsiTreeUtil.getParentOfType(element, KtExpression::class.java)
        if (parent == null || parent.textRange.startOffset < offset) break
        element = parent
    }
    return element
}
