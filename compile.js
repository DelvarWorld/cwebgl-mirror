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
CONNECTION WITH THE SOFTWARE OR THE USE      OR OTHER DEALINGS IN THE SOFTWARE.
*/

(function(glsl, StdIO) {

    /**
     * Import into local scope
     */
    var IRS = glsl.IRS;
    var IR = glsl.IR;
    var sprintf = StdIO.sprintf;

    var irs, ir, swizzles, conditional, state, indent = 0;

    conditional = [];

    //constants
    swizzles = ["xyzw", "rgba", "stpq"];

    /**
     * Constructs a compound statement code block
     *
     * @param   ast_node    ast_node that represents a compound statement type
     */
    function compound_statement(cs) {
        var i, stmt;

        state.symbols.push_scope();
        irs.push( glsl.ast.spacer );
        add('{');
        indent++;
        add( glsl.ast.lineBreak );

        for (i = 0; i < cs.statements.length; i++) {

            stmt = cs.statements[i];

            switch (stmt.typeOf()) {

                case 'ast_expression_statement':
                    expression(stmt.expression);
                    break;

                case 'ast_declarator_list':
                    declarator_list(stmt);
                    break;

                case 'ast_selection_statement':
                    selection_statement(stmt);
                    break;

                default:
                    throw (sprintf("Could not unknown translate statement type %s", stmt.typeOf()), stmt);
            }
        }

        state.symbols.pop_scope();

        indent--;
        add('}');
    }


    /**
     * Constructs a type constructor
     *
     * @param   ast_node    ast_node that represents a constructor operation
     * @param   ast_node    ast_node that represents the constructor components
     */
    function constructor(e, op, se) {
        irs.push( 'Define me constructor!' );
        //var ds, di, si, sei, ses, d, s, ir;

        //ds = glsl.type.size[op.type_specifier];
        //si = 0;
        //sei = 0;

        //e.Type = op.type_specifier;
        //e.Dest = [];

        //e.Dest = irs.getTemp('$tempv');

        //for (di = 0; di < ds; di++) {

            ////build next subexpression
            //if (si === 0) {

                //if (!se[sei]) {
                    //throw ("Not enough parameters to constructor", e);
                //}

                //expression(se[sei]);
                //ses = glsl.type.size[se[sei].Type];
            //}

            ////need to add support for > vec4

            ////compute destination
            //d = e.Dest;
            //d = sprintf("%s.%s", d, swizzles[0][di]);

            ////compute source
            //s = splitOperand(se[sei].Dest);

            ////expression was to just get the identifier, so add the appropriate swizzle,
            ////else, either a number, or the correct swizzle already been set
            //if (s[1]) {
                //s = s.join(".");
            //} else {
                ////value
                //if (s[0].match(/^\-?[0-9]+(\.[0-9]+)?/)) {
                    //s = s[0];
                //} else {
                    //s = sprintf("%s.%s", s[0], swizzles[0][si]);
                //}
            //}

            //ir = new IR('MOV', d, s);
            //irs.push(ir);

            ////used up all components in current expression, move on to the next one
            //si++;
            //if (si >= ses) {
                //si = 0;
                //sei++;
            //}

        //}
    }

    /**
     * Constructs a declaration list
     *
     * @param   ast_node    ast_node that represents a declaration list
     */
    function declarator_list(dl) {
        var type, qualifier, qualifier_name, i, decl, name, entry, constant;

        var out = [ dl.type.specifier.type_name ];

        if( dl.type.qualifier ) {
            out.push( glsl.ast.type_qualifier.flagLookup[ dl.type.qualifier.flags.q ] );
        }

        for (i = 0; i < dl.declarations.length; i++) {
            decl = dl.declarations[i];
            name = decl.identifier;
            out.push( name );
        }

        addLine( space( out ) );
    }

    /**
     * Constructs an expression code block
     *
     * @param   ast_node    ast_node that represents an expression
     */
    function expression(e) {

        if (!e) {
            return;
        }

        //operator
        if (typeof e.oper == 'number') {
            expression_op(e);
            return;
        }

        //simple (variable, or value)
        if (e.primary_expression) {
            expression_simple(e);
            return;
        }

        //cast
        if (e.typeOf('ast_type_specifier')) {
            type_specifier( e );
            //e.Type = e.type_specifier;
            return;
        }

        throw ("Could not translate unknown expression type", e);
    }

    /**
     * Constructs an assignment expression
     *
     * @param   ast_node    ast_node that represents an assignment
     */
    function expression_assign(e, se, local) {
        var cond, ir, temp, size, slots, swz, i, entry;

        irs.push( 'Define expression_assign' );

        //if (e.oper == glsl.ast.operators.add_assign) {
            //se[1].oper = glsl.ast.operators.add;
            //expression_generate(se[1], [se[0], se[1]], 2);
        //}

        //if (conditional.length > 0) {
            //cond = conditional[conditional.length - 1];
        //}

        //if (se[0].Type != se[1].Type) {
            //throw (sprintf("Could not assign value of type %s to %s", glsl.type.names[se[1].Type], glsl.type.names[se[0].Type]), e);
        //}
        //e.Type = se[0].Type;

        //entry = state.symbols.get_variable(se[0].Dest);
        //if (entry.constant) {
            //throw (sprintf("Cannot assign value to constant %s", se[0].Dest), e);
        //}

        //size = glsl.type.size[e.Type];
        //slots = glsl.type.slots[e.Type];

        ////get the swizzle for each slot
        //swz = swizzles[0].substring(0, 4 - (((slots * 4) - size) / slots));

        ////all components are used up in all slots
        //if (swz == swizzles[0]) {
            //swz = "";
        //}

        //for (i = 0; i < slots; i++) {

            //if (cond && !local) {
                //ir = new IR('CMP', se[0].Dest, "-"+cond, se[1].Dest, se[0].Dest);
                //ir.addOffset(i);
                //ir.setSwizzle(swz);
                //irs.push(ir);

            //} else {
                //ir = new IR('MOV', se[0].Dest, se[1].Dest);
                //ir.addOffset(i);
                //ir.setSwizzle(swz);
                //irs.push(ir);
            //}
        //}
    }

    /**
     * Constructs a field selection code block
     *
     * @param   ast_node    ast_node that represents a field selection
     */
    function expression_field(e, se) {
        var field, i, s, swz, new_swz, base, ir, dest, src;

        irs.push( 'Define expression_field' );

        ////pick swizzle set
        //field = e.primary_expression.identifier;
        //for (i = 0; i < swizzles.length; i++) {
            //if (swizzles[i].indexOf(field[0]) != -1) {
                //swz = swizzles[i];
                //break;
            //}
        //}

        ////check that all fields are in same swizzle set
        //if (swz) {
            //new_swz = "";
            //for (i = 0; i < field.length; i++) {
                //s = swz.indexOf(field[i]);
                //if (s == -1) {
                    //swz = false;
                    //break;
                //}
                ////use corresponding "standard" fields (xyzw)
                //new_swz += swizzles[0][s];
            //}
        //}

        //if (swz) {

            //e.Type = makeType(baseType(se[0].Type), new_swz.length);
            //e.Dest = se[0].Dest;

            //if (new_swz.length > 4 || !e.Type) {
                //throw (sprintf("Invalid field selection %s.%s", se[0], e.primary_expression.identifier), e);
            //}

            ////if it's an in-order swizzle, just use the identifier
            //if (swizzles[0].substring(0, new_swz.length) == new_swz) {
                //return;
            //}

            //e.Dest = sprintf("%s.%s", e.Dest, new_swz);
        //}
    }

    /**
     * Constructs a function call expression code block
     *
     * @param   ast_node    ast_node that represents a function call
     */
    function expression_function(e) {
        var i, func, se, def, def_names, dest, entry;

        irs.push( 'Define expression_function' );

        func = e.subexpressions[0].primary_expression.identifier;
        def = [];
        def_names = [];
        dest = [];

        for (i = 0; i < e.expressions.length; i++) {
            se = e.expressions[i];
            expression(se);
            def.push(se.Type);
            def_names.push(glsl.type.names[se.Type]);
            dest.push(se.Dest);
        }

        //entry = glsl.state.symbols.get_function(func, null, def);
        //if (!entry) {
            //throw (sprintf("Function %s(%s) is not defined", func, def_names.join(",")), e);
        //}

        //e.Type = entry.type;
        //e.Dest = IRS.getTemp('$tempv');
        //dest.unshift(e.Dest);

        //parseCode(entry.code, dest);
    }

    /**
     * Constructs the code for an expression
     *
     * @param   int         operation code
     * @param   array       list of subexpressions
     */
    function expression_generate(e, se, len) {
        var table, error, types, dest, i;

        //debugger;
        irs.push( '**' );

        //if (!(table = glsl.ir_operation_table[e.oper])) {
            //throw (sprintf("Could not generate operation %s", glsl.ast.op_names[e.oper]), e);
        //}

        //e.Dest = IRS.getTemp('$tempv');
        //dest = [e.Dest];

        //types = [];
        //for (i = 0; i < len; i++) {
            //types.push(glsl.type.names[se[i].Type]);
            //if (!(table = table[se[i].Type])) {
                //throw sprintf("Could not apply operation %s to %s %s", glsl.ast.op_names[e.oper], types.join(", "), e);
            //}
            //dest.push(se[i].Dest);
        //}

        //e.Type = table.type;

        //if (len <= 4) {
            //e.Dest += sprintf(".%s", swizzles[0].substring(0, glsl.type.size[e.Type]));
        //}

        //parseCode(table.code, dest);
    }

    /**
     * Constructs an operator expression code block
     *
     * @param   ast_node    ast_node that represents an operator expression
     */
    function expression_op(e) {
        var se, temp, ops;

        if (se = e.subexpressions) {
            expression(se[0]);
            expression(se[1]);
            expression(se[2]);
        }

        ops = glsl.ast.operators;

        switch (e.oper) {

            //assignment operator
            case ops.add_assign:
            case ops.assign:
                expression_assign(e, se);
                break;

            case ops.neg:
                if (se[0].Dest[0] != '-') {
                    e.Dest = "-" + se[0].Dest;
                } else {
                    e.Dest = se[0].Dest.substring(1);
                }
                e.Type = se[0].Type;
                break;

            //unary operator
            case ops.logic_not:
                expression_generate(e, se, 1);
                break;

            //binary expression
            case ops.add:
            case ops.sub:
            case ops.mul:
            case ops.div:
            case ops.less:
                expression_generate(e, se, 2);
                break;

            //simple expression
            case ops.int_constant:
            case ops.float_constant:
            case ops.identifier:
                expression_simple(e, se);
                break;

            //function call
            case ops.function_call:
                if (e.cons) {
                    constructor(e, se[0], e.expressions);
                } else {
                    expression_function(e);
                }
                break;

            case ops.field_selection:
                expression_field(e, se);
                break;

            default:
                throw (sprintf("Could not translate unknown expression %s (%s)", e.typeOf(), glsl.ast.op_names[e.oper]), e);
        }
    }

    /**
     * Constructs a simple expression code block
     *
     * @param   ast_node    ast_node that represents a simple expression
     *                      (either an identifier or a single value)
     */
    function expression_simple(e) {
        var name, entry, t;

        //irs.push( 'Defineme expression_simple!' );

        //identifier
        if (e.primary_expression.identifier) {

            irs.push( e.primary_expression.identifier );

            ////lookup identifier in symbol table
            //name = e.primary_expression.identifier;
            //entry = state.symbols.get_variable(name) || state.symbols.get_function(name);

            //if (!entry || !entry.type) {
                //throw (sprintf("%s is undefined", name), e);
            //}

            //e.Type = entry.type;

            //if (entry.constant) {
                //e.Dest = entry.constant;
            //} else {
                //e.Dest = entry.name;
            //}

            return;
            //
        ////float constant
        } else if (typeof e.primary_expression.float_constant != 'undefined') {
            //e.Type = glsl.type.float;
            //e.Dest = makeFloat(e.primary_expression.float_constant);
            return;
        ////int constant
        } else if (typeof e.primary_expression.int_constant != 'undefined') {
            //e.Type = glsl.type.int;
            //e.Dest = makeFloat(e.primary_expression.int_constant);
            return;
        }

        throw ("Cannot translate unkown simple expression type" + e);
    }

    /**
     * Constructs a function header code block
     *
     * @param   ast_node    ast_node that represents an operator expression
     */
    function _function(f) {
        var i, name, param, entry;

        var code = [];

        // void main
        code.push( space( f.return_type.specifier.type_name, f.identifier ) );
        code.push( '(' );

        //generate param list
        for (i = 0; i < f.parameters.length; i++) {
            param = f.parameters[i];
            code.push( param.type.specifier.type_name );
            //if (param.is_void || !param.identifier) {
                //break;
            //}
        }

        code.push( ')' );

        irs.push( code.join('') );

        //generate
        //name = f.identifier;
        //entry = state.symbols.get_function(name);

    }

    /**
     * Constructs a function definition block
     *
     * @param   ast_node    ast_node that represents a function definition
     */
    function function_definition(fd) {

        if (fd.is_definition) {
            //enter definition into symbol table?
            return;
        }

        //handle function proto
        _function(fd.proto_type);

        //handle function body
        compound_statement(fd.body);

        //ir = new IR("RET");
    }

    /**
     * Constructs a selection statement
     *
     * @param   ast_node    Statement
     */
    function selection_statement(stmt) {
        var ir, cond;

        addLineStart( 'if' );
        add( '(' );
        add( glsl.ast.spacer );

        expression(stmt.condition);
        add( ')' );
        add( glsl.ast.spacer );

        //@todo: add a check that condition is bool type?

        cond = sprintf("%s.x", IRS.getTemp('$tempv'));

        //set a flag based on the result
        //ir = new IR('SLT', cond, '0.0', sprintf("%s.x", stmt.condition.Dest));
        //irs.push( 'Dfine me too selection_statement' );

        //if conditional is set, all subsequent output assignments will use the condition result to set using (MAD dest, cond, (new - old), old)
        conditional.push(cond);
        compound_statement(stmt.then_statement);

        if (stmt.else_statement) {

            irs.push( glsl.ast.spacer );
            irs.push('else');
            irs.push( glsl.ast.spacer );

            expression(stmt.condition);

            //ir = new IR('SGE', cond, "0.0", cond);
            //irs.push(ir);
            compound_statement(stmt.else_statement);
        }

        conditional.pop();
    }

    /**
     * Constructs a translation unit
     *
     * @param   ast_node    ast_node that represents a translation unit
     */
    function translation_unit(tu) {
        switch (tu.typeOf()) {
            case 'ast_declarator_list':
                declarator_list(tu);
                break;
            case 'ast_function_definition':
                function_definition(tu);
                break;
            case 'ast_type_specifier':
                type_specifier(tu);
                break;
            default:
                throw (sprintf('Unknown translation unit %s', tu.typeOf()), tu);
        }
    }

    /**
     * Constructs a type specifier code block
     *
     * @param   ast_node    ast_node that represents a type specifier
     */
    function type_specifier(ts) {
        var precision = glsl.ast.precisionLookup[ ts.precision ],
            out = [];

        if( precision && precision !== 'none' ) {
            out.push( 'precision', precision );
        }

        irs.push(
            space( out.concat( ts.type_name ) )
        );
    }

    /**
     * Constructs an error message
     *
     * @param   string      The error message
     * @param   ast_node    The error ast_node
     *
     * @return  string
     */
    function throw_error (msg, n) {
        if (n && n.location) {
            msg = sprintf("%s at line %s, column %s", msg, n.location.line, n.location.column);
        }
        throw new Error(msg);
    }

    /**
     * Makes number a float representation
     *
     * @param   string      The string representation of a number
     *
     * @return  string
     */
    function makeFloat(n) {
        n += (n.toString().indexOf('.') == -1) ? ".0" : "";
        return n;
    }

    /**
     * Returns the base type of the type
     *
     * @param   integer     The base type (float, int...)
     * @param   size        The size to construct
     *
     * @return  string
     */
    function makeType(base, size) {
        var name;

        if (size == 1) {
            return base;
        }

        if (size <= 4) {
            if (base == glsl.type.float) {
                return glsl.type.vec2 + (size - 2);
            }
        }

        return null;
    }

    /**
     * Returns a new type using base type and size
     *
     * @param   integer     The base type (float, int...)
     * @param   size        The size to construct
     *
     * @return  string
     */
    function baseType(type) {
        return glsl.type.base[type];
    }

    /**
     * Splits an operand into name and swizzle parts
     *
     * @param   string      The operand
     *
     * @return  array       [name, swizzle]
     */
    function splitOperand(oprd) {
        oprd = oprd.split(".");
        //if (!oprd[1] || oprd[1].match(/[xyzw]+/)) {

        //} else {
            oprd = [oprd.join(".")];
        //}
        return oprd;
    }

    /**
     * Builds instructions from code table record
     *
     * @param   array       List of instruction strings
     * @param   array       List of operands
     */
    function parseCode(code, oprds) {
        var repl, dest, parts, i, j, oprd, ir, new_swz;

        for (i = 0; i < oprds.length; i++) {
            oprd = splitOperand(oprds[i]);
            if (oprd[1]) {
                //need a new temp to move the swizzle so our code pattern works
                new_swz = swizzles[0].substring(0, oprd[1].length);
                if (oprd[1] != new_swz) {
                    dest = irs.getTemp('$tempv');
                    ir = new IR('MOV', sprintf("%s.%s", dest, new_swz), oprd.join("."));
                    irs.push(ir);
                    oprd[0] = dest;
                }
            }
            oprds[i] = oprd[0];
        }

        repl = [];
        for (i = oprds.length - 1; i >= 0; i--) {
            repl.push({
                s : new RegExp('%' + (i + 1), 'g'),
                d : oprds[i]
            });
        }

        for (i = 0; i < code.length; i++) {
            parts = code[i];

            if (parts.substring(0, 4) == 'TEMP') {
                repl.unshift({
                    s : new RegExp(parts.substring(5), 'g'),
                    d : IRS.getTemp('$tempv')
                });
                continue;
            }

            for (j = 0; j < repl.length; j++) {
                parts = parts.replace(repl[j].s, repl[j].d);
            }

            parts = parts.split(" ");

            irs.push(new IR(parts[0], parts[1], parts[2], parts[3], parts[4]));
        }
    }

    /**
     * Construct from symbol table
     *
     * @param   object    Symbol table
     */
    function symbols( symbolTable ) {
        var i, entry;
        for (i in  symbolTable .table) {
            entry = symbolTable .table[i];
            //if (entry.typedef == glsl.symbol_table_entry.typedef.variable) {
                //console.log(entry);
            //}
        }
    }

    /**
     * Constructs a program's object code from an ast and symbol table
     *
     * @param   string      The error message
     * @param   ast_node    The error ast_node
     *
     * @return  string
     */
    function generateShader(new_state) {
        var i;

        state = new_state;
        irs = new IRS();

        symbols(state.symbols);

        for (i = 0; i < state.translation_unit.length; i++) {
            translation_unit(state.translation_unit[i]);
        }

        return irs;
    }

    // TODO: We could cache this
    function insideOut( object ) {
        var reversed = {};

        for( var key in object ) {
            reversed[ object[ key ] ] = key;
        }

        return reversed;
    }

    function space() {
        return Array.prototype.join.call( typeof arguments[0] === 'object' ? arguments[0] : arguments, ' ' );
    }

    glsl.ast.precisionLookup = insideOut( glsl.ast.precision );
    glsl.ast.type_qualifier.flagLookup = insideOut( glsl.ast.type_qualifier.flags );

    /**
     * External interface
     */
    glsl.generateShader = generateShader;
    glsl.ast.spacer = ' ';
    glsl.ast.indent = '    ';
    glsl.ast.currentIndent = '';
    glsl.ast.lineBreak = '\n';
    glsl.ast.lineTerminator = ';' + glsl.ast.lineBreak;

    function add( code ) {
        irs.push( code );
    }

    function addLineStart( code ) {
        var indent = '';

        for( var x = 0; x < indent; x++ ) {
            indent += glsl.ast.indent;
        }

        add( indent );
        add( code );
    }

    function addLine( code, unterimnated ) {
        addLineStart( code );
        
        if( !unterimnated ) {
            irs.push( glsl.ast.lineTerminator );
        }
    }

}(glsl, StdIO));
