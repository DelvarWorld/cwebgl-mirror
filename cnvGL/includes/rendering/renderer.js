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

cnvgl_renderer = (function() {

	//Internal Constructor
	function Initializer() {
		//public:
		this.state = null;
		this.program = null;
		this.vertex = null;
		this.fragment = null;
		this.clipping = null;
	}

	var cnvgl_renderer = jClass('cnvgl_renderer', Initializer);

	//public:

	cnvgl_renderer.cnvgl_renderer = function() {
		this.vertex = new cnvgl_rendering_vertex(this);
		this.fragment = new cnvgl_rendering_fragment(this);
		this.clipping = new cnvgl_rendering_clipping(this);
	};

	cnvgl_renderer.setProgram = function(program) {
		this.program = program;	
		this.vertex.setProgram(program.vertex_program);
		this.fragment.setProgram(program.fragment_program);
	};

	cnvgl_renderer.send = function(prim) {
		var i;
		for (i = 0; i < prim.vertices.length; i++) {
			this.vertex.process(prim.vertices[i]);
		}
		this.render(prim);
	};

	cnvgl_renderer.render = function(prim) {
		this.renderTriangles(prim.vertices);
	};

	cnvgl_renderer.renderTriangles = function(vertices) {
		var i, prim;
		for (i = 0; i < vertices.length - 2; i++) {

			prim = new cnvgl_primitive();
			prim.vertices[0] = vertices[i];
			prim.vertices[1] = vertices[i + 1];
			prim.vertices[2] = vertices[i + 2];
			
			this.clipping.clip(prim);

			this.renderTriangle(prim);
		}
	};

	cnvgl_renderer.renderTriangle = function(prim) {

		var v1, v2, v3, dir;
		var lsteps, rsteps, ysteps;
		var frag, varying;

		//prepare (sort) vertices
		this.vertex.sortVertices(prim);
		if (!prim.direction) {
			prim.direction = this.vertex.getDirection(prim.vertices);
		}

		dir = prim.direction;
		v1 = prim.vertices[0];
		if (dir > 0) {
			v2 = prim.vertices[1];
			v3 = prim.vertices[2];
		} else {
			v2 = prim.vertices[2];
			v3 = prim.vertices[1];				
		}

		frag = new cnvgl_fragment();
		varying = new cnvgl_rendering_varying(v1, v2, v3);

		lsteps = this.vertex.slope(v1.sx, v1.sy, v3.sx, v3.sy);
		rsteps = this.vertex.slope(v1.sx, v1.sy, v2.sx, v2.sy);

		var yi_start, yi_end, yi, yp = false, x_start, x_end;

		//top and bottom
		yi_start = Math.ceil(v1.sy - 0.5) + 1;
		yi_end = Math.ceil((v2.sy > v3.sy ? v2.sy : v3.sy) + 0.5) - 1;
		x_start = v1.sx;
		x_end = v1.sx;

		//top line is horizontal, "fix" x_end
		if (v1.sy == v2.sy) {
			x_end = v2.sx;
		}

		//for each horizontal scanline
		for (yi = yi_start; yi < yi_end; yi++) {

			//next vertex (v1, v2) -> (v2, v3)
			if (!yp && yi > v2.sy) {
				lsteps = this.vertex.slope(v2.sx, v2.sy, v3.sx, v3.sy);
				if (v1.sy != v2.sy) {
					lsteps.x = -lsteps.x;
				}
				yp = true;
			}

			//next vertex (v1, v3) -> (v2, v3)
			if (!yp && yi > v3.sy) {
				rsteps = this.vertex.slope(v2.sx, v2.sy, v3.sx, v3.sy);
				if (v1.sy != v2.sy) {
					rsteps.x = -rsteps.x;
				}
				yp = true;
			}

			x_start += lsteps.x;
			x_end += rsteps.x;

			this.processScanline(yi, x_start, x_end, frag, varying, [v1, v2, v3]);
		}
	};

	cnvgl_renderer.processScanline = function(yi, x_start, x_end, frag, varying, verts) {
		var buffer, vw, xi, i, v, p;

		buffer = this.state.color_buffer;
		depth = this.state.depth_buffer;

		vw = this.state.viewport_w;

		for (xi = Math.floor(x_start); xi < x_end; xi++) {

			i = (vw * yi + xi);

			p = [xi, yi, 0, 1];
			varying.prepare(frag, p);

			frag.gl_FragDepth = varying.interpolate(verts[0].z, verts[1].z, verts[2].z);

			if (frag.gl_FragDepth < depth[i]) {
				continue;
			}

			//interpolate varying
			for (v in varying.varying) {
				frag.varying[v] = varying.interpolate(varying.f1[v], varying.f2[v], varying.f3[v]);
			}

			this.fragment.process(frag);

			depth[i] = frag.gl_FragDepth;

			i *= 4;
			buffer[i] = frag.r;
			buffer[i + 1] = frag.g;
			buffer[i + 2] = frag.b;
			//if (alpha, do calculation next)
		}		
	};


	return cnvgl_renderer.Constructor;

}());

