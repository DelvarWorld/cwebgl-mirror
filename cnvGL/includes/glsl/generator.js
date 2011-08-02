/*
Copyright (c) 2011 Cimaron Shanahan

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

glsl.generator = (function() {

	//-------------------------------------------------
	//	Code Generation Options/Data
	//-------------------------------------------------

	//Type qualifier global variables
	var g_type_qualifier_globals = [];
	g_type_qualifier_globals[glsl.ast.type_qualifier.flags.varying] = '__varying';

	function g_type_default_value(type) {
		switch (type.type_specifier) {
			case glsl.ast.types.vec4:
				return '[0,0,0,0]';
			default:
				return g_error('Cannot generate default value for type ' + type.type_name, type);
		}
	}

	function g_type_qualifier_global(qualifier) {
		var qual = '';
		for (var i in g_type_qualifier_globals) {
			if (qualifier.flags.q & i) {
				qual = g_type_qualifier_globals[i];
			}
		}
		return qual;
	}

	function g_get_type(t) {
		//@todo:
		//lookup type in symbol table
		//if not found, return undefined identifier
		//return type
		return 'generic_type';
	}

	function g_indent() {
		return new Array(generator.depth + 1).join("\t");
	}

	//-------------------------------------------------
	//	Code Generation
	//-------------------------------------------------




	function g_ast_type_specifier(ts) {
		if (ts.is_precision_statement) {
			return "\n";	
		}
		return g_error('Cannot generate default value for type ' + type, ts);
	}

	function g_ast_declarator_list(dl) {
		
		var code = '', q_code = '', d_code;
		
		//generate qualifier global vars for external communication
		var type = dl.type;
		var q = type.has_qualifiers();
		if (q) {
			q_code = g_type_qualifier_global(type.qualifier);
		}

		//get default initialization values
		var specifier = type.specifier;
		d_code = g_type_default_value(specifier);
		if (!d_code) {
			return false;
		}

		var list = dl.declarations;
		for (var i = 0; i < list.length; i++) {
			var decl = list[i];
			var name = decl.identifier;
			//qualifier?[name] = default_value
			code += (q ? (q_code + "['" + name + "']") : name) + " = " + d_code + ";\n";			
		}
		return code;
	}

	function g_ast_function(f) {
		var code = '', p_code = '';

		var name = f.identifier;
		var parameters = f.parameters;

		var params = [];
		for (var i = 0; i < parameters.length; i++) {
			var param = parameters[i];
			if (param.is_void) {
				return '';
			}
			params.push(param.identifier);
		}
		p_code = params.join(", ");

		code = "function " + name + "(" + p_code + ")";

		return code;
	}

	function g_ast_expression(e) {
		var code = '';
		var op = e.oper;
		var se = e.subexpressions;
		var se1 = se[0], se2 = se[1], se3 = se[2];
		
		var t1 = g_get_type(se1), t2 = g_get_type(se2), t3 = g_get_type(se3);
		
		switch (op) {
			case glsl.ast.operators.assign:
				if (t1 != t2) {
					return g_error("Could not assign value of type " + t2 + " to " + t1, e);
				}
				var left = se1.primary_expression.identifier;
				var right = se2.primary_expression.identifier;
				
				//@todo:
				//check that left is a valid identifier
				//if left has a quantifier, generate that
				//get generalized value from right
				code = left + " = " + right;
				return code;
			default:
				return g_error("Could not translate operation " + op, e);
		}
	}

	function g_ast_expression_statement(es) {
		var code = '';
		code = g_ast_expression(es.expression);
		if (code) {
			code += ";\n";
		}
		return code;
	}

	function g_ast_compound_statement(cs) {
		var code = '';
		var stmts = cs.statements;
		generator.depth++;
		for (var i = 0; i < stmts.length; i++) {
			var stmt = stmts[i];
			switch (stmt.typeof()) {
				case 'ast_expression_statement':
					var es = g_ast_expression_statement(stmt)
					if (!es) {
						return false;	
					}
					code += g_indent() + es;
					break;
				default:
					return g_error("Could not translate statement type (" + stmt.typeof() + ")", stmt);
			}
		}
		
		generator.depth--;
		code = g_indent() + "{\n" + code + g_indent() + "}\n";
		return code;
	}

	function g_ast_function_definition(fd) {
		var code = '', p_code = '', b_code = '';
		
		if (fd.is_definition) {
			//don't need to define functions beforehand
			return "\n";
		}

		p_code = g_ast_function(fd.proto_type);
		b_code = g_ast_compound_statement(fd.body);
		if (!b_code) {
			return false;	
		}

		code = p_code + "\n" + b_code;
		return code;
	}

	function g_translation_unit(tu) {
		var t = tu.typeof();
		switch (t) {
			case 'ast_declarator_list':
				return g_ast_declarator_list(tu);
			case 'ast_type_specifier':
				return g_ast_type_specifier(tu);
			case 'ast_function_definition':
				return g_ast_function_definition(tu);
			default:
				return g_error('Cannot translate syntax tree node (' + d.typeof() + ')'  , tu);
		}
	}

	
	function g_error(msg, n) {
		generator.errorMsg = msg;
		if (n && n.location) {
			generator.errorMsg += " at line " + n.location.line + ", column " + n.location.column;	
		}
		generator.status = false;
		return false;
	}
	
	var generator = {
		
		depth : 0,
		status : false,
		objectCode : '',
		errorMsg : '',

		createObjectCode : function(state) {

			this.objectCode = '';
			this.errorMsg = '';
			this.status = false;

			for (var i = 0; i < state.translation_unit.length; i++) {

				var tu = state.translation_unit[i];

				var res = g_translation_unit(tu);
				if (res) {
					this.objectCode += res;	
				} else {
					return false;
				}
			}

			this.status = true;
			return true;
		}
		
		
	};

	return generator;
})();