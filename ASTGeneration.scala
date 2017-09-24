package mc.astgen
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext
import java.io.{PrintWriter,File}
import org.antlr.v4.runtime.ANTLRFileStream
import mc.utils._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree._
import mc.parser._
import mc.parser.MCParser._

class ASTGeneration extends MCBaseVisitor[Any] {

	override def visitProgram(ctx:ProgramContext) = 
		Program(ctx.decl.asScala.toList.foldLeft(List[Decl]())
												((a,b) => a:::b.accept(this).asInstanceOf[List[Decl]]))

		override def visitDecl(ctx:DeclContext) =
			if (ctx.vardecl != null)
				ctx.vardecl.accept(this)
			else
				List(ctx.fundecl.accept(this))

		override def visitVardecl(ctx:VardeclContext) =
			ctx.varname.asScala.toList.map(x =>
				VarDecl(Id(x.ID.getText),
					if (x.INTLIT != null)
						ArrayType(IntLiteral(x.INTLIT.getText.toInt),
								  ctx.primtype.accept(this).asInstanceOf[Type])
					else
						ctx.primtype.accept(this).asInstanceOf[Type]
				)
			)

		override def visitFundecl(ctx:FundeclContext) =
				FuncDecl(Id(ctx.ID.getText),
						 if (ctx.paralist != null) ctx.paralist.accept(this).asInstanceOf[List[VarDecl]] else List(),
						 ctx.rettype.accept(this).asInstanceOf[Type],
						 ctx.blockstmt.accept(this).asInstanceOf[Stmt])

		override def visitRettype(ctx:RettypeContext) =
				if (ctx.VOIDTYPE != null) VoidType
				else ctx.getChild(0).accept(this)

		override def visitPrimtype(ctx:PrimtypeContext) = 
				if 		(ctx.INTTYPE != null) 	IntType
				else if (ctx.FLOATTYPE != null) FloatType
				else if (ctx.BOOLTYPE != null) 	BoolType
				else 							StringType

		override def visitArrtype(ctx:ArrtypeContext) = 
				ArrayType(IntLiteral(ctx.INTLIT.getText.toInt),
							ctx.primtype.accept(this).asInstanceOf[Type])

		override def visitArrptrtype(ctx: ArrptrtypeContext) =
				ArrayPointerType(ctx.primtype.accept(this).asInstanceOf[Type])

		override def visitParalist(ctx:ParalistContext) =
				ctx.para.asScala.toList.map(_.accept(this).asInstanceOf[VarDecl])

		override def visitPara(ctx:ParaContext) =
				if (ctx.getChildCount == 2)
						VarDecl(Id(ctx.ID.getText), ctx.primtype.accept(this).asInstanceOf[Type])
				else
						VarDecl(Id(ctx.ID.getText), ArrayPointerType(ctx.primtype.accept(this).asInstanceOf[Type]))

		override def visitBlockstmt(ctx:BlockstmtContext) = 
				Block(ctx.vardecl.asScala.toList.foldLeft(List[VarDecl]())((a,b) => a:::b.accept(this).asInstanceOf[List[VarDecl]]),
					  ctx.stmt.asScala.toList.map(_.accept(this).asInstanceOf[Stmt]))

				// Program(ctx.decl.asScala.toList.foldLeft(List[Decl]())
				// 								((a,b) => a:::b.accept(this).asInstanceOf[List[Decl]]))

		override def visitStmt(ctx:StmtContext) =
				if (ctx.matchstmt != null) ctx.matchstmt.accept(this)
				else ctx.unmatchstmt.accept(this)

		override def visitMatchstmt(ctx:MatchstmtContext) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					If(ctx.exp.accept(this).asInstanceOf[Expr],
					   ctx.matchstmt(0).accept(this).asInstanceOf[Stmt],
					   Some(ctx.matchstmt(1).accept(this).asInstanceOf[Stmt]))

		override def visitUnmatchstmt(ctx:UnmatchstmtContext) =
				if (ctx.getChildCount == 5)
					If(ctx.exp.accept(this).asInstanceOf[Expr],
					   ctx.stmt.accept(this).asInstanceOf[Stmt],
					   None)
				else
					If(ctx.exp.accept(this).asInstanceOf[Expr],
					   ctx.matchstmt.accept(this).asInstanceOf[Stmt],
					   Some(ctx.unmatchstmt.accept(this).asInstanceOf[Stmt]))

		override def visitDowhilestmt(ctx:DowhilestmtContext) =
				Dowhile(ctx.stmt.asScala.toList.map(_.accept(this).asInstanceOf[Stmt]),
							ctx.exp.accept(this).asInstanceOf[Expr])

		override def visitForstmt(ctx:ForstmtContext) =
				For(ctx.exp(0).accept(this).asInstanceOf[Expr],
					ctx.exp(1).accept(this).asInstanceOf[Expr],
					ctx.exp(2).accept(this).asInstanceOf[Expr],
					ctx.stmt.accept(this).asInstanceOf[Stmt])

		override def visitBrkstmt(ctx:BrkstmtContext) =
				Break

		override def visitContstmt(ctx:ContstmtContext) =
				Continue

		override def visitRetstmt(ctx:RetstmtContext) =
				Return(if (ctx.exp != null) Some(ctx.exp.accept(this).asInstanceOf[Expr]) else None)

		override def visitExpstmt(ctx:ExpstmtContext) =
				ctx.exp.accept(this)

		override def visitExp(ctx:ExpContext) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					BinaryOp(ctx.getChild(1).getText,
							 ctx.getChild(0).accept(this).asInstanceOf[Expr],
							 ctx.getChild(2).accept(this).asInstanceOf[Expr])

		override def visitExp1(ctx:Exp1Context) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					BinaryOp(ctx.getChild(1).getText,
							 ctx.getChild(0).accept(this).asInstanceOf[Expr],
							 ctx.getChild(2).accept(this).asInstanceOf[Expr])

		override def visitExp2(ctx:Exp2Context) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					BinaryOp(ctx.getChild(1).getText,
							 ctx.getChild(0).accept(this).asInstanceOf[Expr],
							 ctx.getChild(2).accept(this).asInstanceOf[Expr])

		override def visitExp3(ctx:Exp3Context) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					BinaryOp(ctx.getChild(1).getText,
							 ctx.getChild(0).accept(this).asInstanceOf[Expr],
							 ctx.getChild(2).accept(this).asInstanceOf[Expr])

		override def visitExp4(ctx:Exp4Context) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					BinaryOp(ctx.getChild(1).getText,
							 ctx.getChild(0).accept(this).asInstanceOf[Expr],
							 ctx.getChild(2).accept(this).asInstanceOf[Expr])

		override def visitExp5(ctx:Exp5Context) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					BinaryOp(ctx.getChild(1).getText,
							 ctx.getChild(0).accept(this).asInstanceOf[Expr],
							 ctx.getChild(2).accept(this).asInstanceOf[Expr])

		override def visitExp6(ctx:Exp6Context) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					BinaryOp(ctx.getChild(1).getText,
							 ctx.getChild(0).accept(this).asInstanceOf[Expr],
							 ctx.getChild(2).accept(this).asInstanceOf[Expr])	

		override def visitExp7(ctx:Exp7Context) =
				if (ctx.getChildCount == 1) ctx.getChild(0).accept(this)
				else
					UnaryOp(ctx.getChild(0).getText,
							ctx.getChild(1).accept(this).asInstanceOf[Expr])	

		override def visitExp8(ctx:Exp8Context) =
				if (ctx.getChildCount == 3) ctx.exp.accept(this)
				else if (ctx.literal != null) ctx.literal.accept(this)
				else if (ctx.funcall != null) ctx.funcall.accept(this)
				else if (ctx.ID != null) Id(ctx.ID.getText)
				else ArrayCell(ctx.exp8.accept(this).asInstanceOf[Expr],
							   ctx.exp.accept(this).asInstanceOf[Expr])

		override def visitLiteral(ctx:LiteralContext) =
				if (ctx.INTLIT != null) IntLiteral(ctx.INTLIT.getText.toInt)
				else if (ctx.FLOATLIT != null) FloatLiteral(ctx.FLOATLIT.getText.toFloat)
				else if (ctx.STRINGLIT != null) StringLiteral(ctx.STRINGLIT.getText)
				else BooleanLiteral(ctx.BOOLLIT.getText.toBoolean)

		override def visitFuncall(ctx:FuncallContext) = 
				CallExpr(Id(ctx.ID.getText),
						 ctx.exp.asScala.toList.map(_.accept(this).asInstanceOf[Expr]))
}