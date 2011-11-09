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

/*
 * Class cnvgl_rendering_program
 * Creates a runtime environment for shader programs.
 *
 */
cnvgl_rendering_program = (function() {

	function Initializer() {

		//public:
		this._ctx = null;
		this._renderer = null;

		//vertex/fragment objects
		this.vertex = null;
		this.fragment = null;

		//data sharing
		this._out = null;
		this._varying = null;
		this._uniforms = null;
	}

	var cnvgl_rendering_program = jClass('cnvgl_rendering_program', Initializer);	

	cnvgl_rendering_program.cnvgl_rendering_program = function(ctx, renderer) {
		this._ctx = ctx;
		this._renderer = renderer;
		this._texture2D = renderer.texture.texture2D;
	};

	cnvgl_rendering_program.setProgram = function(__code) {
		eval(__code);
		return main;
	};

	//--------------------------------------------------------------------
	//	The following implements the math library for glsl programs.
	//	Most of the code is taken from or derived from glMatrix.js library
	//--------------------------------------------------------------------
	var vec2 = {}, vec3 = {}, vec4 = {}, mat2 = {}, mat3 = {}, mat4 = {};

	/*
	 * vec3.add
	 * Performs a vector addition
	 *
	 * Params:
	 * vec - vec3, first operand
	 * vec2 - vec3, second operand
	 *
	 * Returns:
	 * vec3
	 */
	vec3.add = function(vec, vec2, dest) {
		var dest = [];
		dest[0] = vec[0] + vec2[0];
		dest[1] = vec[1] + vec2[1];
		dest[2] = vec[2] + vec2[2];
		return dest;
	};

	/*
	 * vec3.multiply
	 * Performs a vector multiplication
	 *
	 * Params:
	 * vec - vec3, first operand
	 * vec2 - vec3, second operand
	 *
	 * Returns:
	 * vec3
	 */
	vec3.multiply = function(vec, vec2) {
		var dest = [];
		dest[0] = vec[0] * vec2[0];
		dest[1] = vec[1] * vec2[1];
		dest[2] = vec[2] * vec2[2];
		return dest;
	};

	/*
	 * vec3.normalize
	 * Generates a unit vector of the same direction as the provided vec3
	 * If vector length is 0, returns [0, 0, 0]
	 *
	 * Params:
	 * vec - vec3 to normalize
	 *
	 * Returns:
	 * vec3
	 */
	vec3.normalize = function(vec) {
		var dest = [];
		
		var x = vec[0], y = vec[1], z = vec[2];
		var len = Math.sqrt(x*x + y*y + z*z);

		if (!len) {
			dest[0] = 0;
			dest[1] = 0;
			dest[2] = 0;
			return dest;
		} else if (len == 1) {
			dest[0] = x;
			dest[1] = y;
			dest[2] = z;
			return dest;
		}

		len = 1 / len;
		dest[0] = x*len;
		dest[1] = y*len;
		dest[2] = z*len;
		return dest;
	};

	/*
	 * vec3.scale
	 * Multiplies the components of a vec3 by a scalar value
	 *
	 * Params:
	 * vec - vec3 to scale
	 * val - Numeric value to scale by
	 *
	 * Returns:
	 * vec3
	 */
	vec3.scale = function(vec, val) {
		var dest = [];
		dest[0] = vec[0]*val;
		dest[1] = vec[1]*val;
		dest[2] = vec[2]*val;
		return dest;
	};

	/*
	 * vec3.dot
	 * Caclulates the dot product of two vec3s
	 *
	 * Params:
	 * vec - vec3, first operand
	 * vec2 - vec3, second operand
	 *
	 * Returns:
	 * Dot product of vec and vec2
	 */
	vec3.dot = function(vec, vec2){
		return vec[0]*vec2[0] + vec[1]*vec2[1] + vec[2]*vec2[2];
	};

	/*
	 * vec4.add
	 * Performs a vector addition
	 *
	 * Params:
	 * vec - vec4, first operand
	 * vec2 - vec4, second operand
	 *
	 * Returns:
	 * vec4
	 */
	vec4.add = function(vec, vec2, dest) {
		var dest = [];
		dest[0] = vec[0] + vec2[0];
		dest[1] = vec[1] + vec2[1];
		dest[2] = vec[2] + vec2[2];
		dest[3] = vec[3] + vec2[3];
		return dest;
	};

	/*
	 * vec4.dot
	 * Caclulates the dot product of two vec4s
	 *
	 * Params:
	 * vec - vec4, first operand
	 * vec2 - vec4, second operand
	 *
	 * Returns:
	 * Dot product of vec and vec2
	 */
	vec4.dot = function(vec, vec2){
		return vec[0]*vec2[0] + vec[1]*vec2[1] + vec[2]*vec2[2] + vec[3]*vec2[3];
	};
	
	/*
	 * vec4.multiply
	 * Performs a vector multiplication
	 *
	 * Params:
	 * vec - vec4, first operand
	 * vec2 - vec4, second operand
	 *
	 * Returns:
	 * vec4
	 */
	vec4.multiply = function(vec, vec2) {
		var dest = [];
		dest[0] = vec[0] * vec2[0];
		dest[1] = vec[1] * vec2[1];
		dest[2] = vec[2] * vec2[2];
		dest[3] = vec[3] * vec2[3];
		return dest;
	};
	
	/*
	 * vec4.normalize
	 * Generates a unit vector of the same direction as the provided vec4
	 * If vector length is 0, returns [0, 0, 0, 0]
	 *
	 * Params:
	 * vec - vec4 to normalize
	 *
	 * Returns:
	 * vec4
	 */
	vec4.normalize = function(vec) {
		var dest = [];

		var x = vec[0], y = vec[1], z = vec[2], w = vec[3];
		var len = Math.sqrt(x*x + y*y + z*z + w*w);

		if (!len) {
			dest[0] = 0;
			dest[1] = 0;
			dest[2] = 0;
			dest[3] = 0;
			return dest;
		} else if (len == 1) {
			dest[0] = x;
			dest[1] = y;
			dest[2] = z;
			dest[3] = w;
			return dest;
		}
		
		len = 1 / len;
		dest[0] = x*len;
		dest[1] = y*len;
		dest[2] = z*len;
		dest[3] = w*len;
		return dest;
	};

	/*
	 * vec4.reflect
	 * For the incident vector I and surface orientation N, returns the reflection direction:
	 * I � 2 * dot(N, I) * N
	 * N must already be normalized in order to achieve the desired result.
	 *
	 * Params:
	 * vec - vec4, first operand
	 * vec2 - vec4, second operand
	 *
	 * Returns:
	 * vec4
	 */
	vec4.reflect = function(vec1, vec2) {
		return vec4.sub(vec1, vec4.scale(vec2, 2 * vec4.dot(vec2, vec1)));
	};

	/*
	 * vec4.sub
	 * Performs a vector subtraction
	 *
	 * Params:
	 * vec - vec4, first operand
	 * vec2 - vec4, second operand
	 *
	 * Returns:
	 * vec4
	 */
	vec4.sub = function(vec, vec2) {
		var dest = [];
		dest[0] = vec[0] - vec2[0];
		dest[1] = vec[1] - vec2[1];
		dest[2] = vec[2] - vec2[2];
		dest[3] = vec[3] - vec2[3];
		return dest;
	};

	/*
	 * mat3.multiplyVec3
	 * Transforms a vec3 with the given matrix
	 *
	 * Params:
	 * mat - mat3 to transform the vector with
	 * vec - vec3 to transform
	 *
	 * Returns:
	 * vec3
	 */
	mat3.multiplyVec3 = function(mat, vec) {
		var dest = [];
		var x = vec[0], y = vec[1], z = vec[2];
		dest[0] = mat[0]*x + mat[3]*y + mat[6]*z;
		dest[1] = mat[1]*x + mat[4]*y + mat[7]*z;
		dest[2] = mat[2]*x + mat[5]*y + mat[8]*z;

		return dest;
	};

	/*
	 * mat4.multiply
	 * Performs a matrix multiplication
	 *
	 * Params:
	 * mat - mat4, first operand
	 * mat2 - mat4, second operand
	 *
	 * Returns:
	 * mat4
	 */
	mat4.multiply = function(mat, mat2) {
		var dest = [];
        
        // Cache the matrix values (makes for huge speed increases!)
        var a00 = mat[0], a01 = mat[1], a02 = mat[2], a03 = mat[3];
        var a10 = mat[4], a11 = mat[5], a12 = mat[6], a13 = mat[7];
        var a20 = mat[8], a21 = mat[9], a22 = mat[10], a23 = mat[11];
        var a30 = mat[12], a31 = mat[13], a32 = mat[14], a33 = mat[15];
        
        var b00 = mat2[0], b01 = mat2[1], b02 = mat2[2], b03 = mat2[3];
        var b10 = mat2[4], b11 = mat2[5], b12 = mat2[6], b13 = mat2[7];
        var b20 = mat2[8], b21 = mat2[9], b22 = mat2[10], b23 = mat2[11];
        var b30 = mat2[12], b31 = mat2[13], b32 = mat2[14], b33 = mat2[15];
        
        dest[0] = b00*a00 + b01*a10 + b02*a20 + b03*a30;
        dest[1] = b00*a01 + b01*a11 + b02*a21 + b03*a31;
        dest[2] = b00*a02 + b01*a12 + b02*a22 + b03*a32;
        dest[3] = b00*a03 + b01*a13 + b02*a23 + b03*a33;
        dest[4] = b10*a00 + b11*a10 + b12*a20 + b13*a30;
        dest[5] = b10*a01 + b11*a11 + b12*a21 + b13*a31;
        dest[6] = b10*a02 + b11*a12 + b12*a22 + b13*a32;
        dest[7] = b10*a03 + b11*a13 + b12*a23 + b13*a33;
        dest[8] = b20*a00 + b21*a10 + b22*a20 + b23*a30;
        dest[9] = b20*a01 + b21*a11 + b22*a21 + b23*a31;
        dest[10] = b20*a02 + b21*a12 + b22*a22 + b23*a32;
        dest[11] = b20*a03 + b21*a13 + b22*a23 + b23*a33;
        dest[12] = b30*a00 + b31*a10 + b32*a20 + b33*a30;
        dest[13] = b30*a01 + b31*a11 + b32*a21 + b33*a31;
        dest[14] = b30*a02 + b31*a12 + b32*a22 + b33*a32;
        dest[15] = b30*a03 + b31*a13 + b32*a23 + b33*a33;

        return dest;
	};
	
	/*
	 * mat4.multiplyVec4
	 * Transforms a vec4 with the given matrix
	 *
	 * Params:
	 * mat - mat4 to transform the vector with
	 * vec - vec4 to transform
	 *
	 * Returns:
	 * vec4
	 */
	mat4.multiplyVec4 = function(mat, vec) {
		var dest = [];
		
		var x = vec[0], y = vec[1], z = vec[2], w = vec[3];
		
		dest[0] = mat[0]*x + mat[4]*y + mat[8]*z + mat[12]*w;
		dest[1] = mat[1]*x + mat[5]*y + mat[9]*z + mat[13]*w;
		dest[2] = mat[2]*x + mat[6]*y + mat[10]*z + mat[14]*w;
		dest[3] = mat[3]*x + mat[7]*y + mat[11]*z + mat[15]*w;
		
		return dest;
	};


	return cnvgl_rendering_program.Constructor;

}());

