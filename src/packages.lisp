(defpackage #:gmsh
  (:use #:common-lisp)
  (:nicknames #:cl-gmsh)
  (:export
    #:v? #:d? #:i? #:*opt* #:*opt1* #:*eps* #:*programs* #:*matpar* #:doc
    #:*wrncnt* #:wrn #:inline?
    #:with-fast-stack #:make-fast-stack #:stack #:stack-ind
    #:gmsh #:make-bvh #:clear! #:center!

    #:@vert #:@verts #:@uv #:@uvs #:@norm #:@norms
    #:@connected-verts
    #:@vnum #:@pnum #:@vmax #:@poly-edges #:@edge-polys #:@poly #:@all-polys

    #:add-poly! #:add-vert! #:add-verts! #:add-polys!
    #:add-plane! #:add-box! #:del-poly! #:split-edge!
    #:tx!
    #:msk/tx! #:msk/v #:msk/set!
    #:norm-poly #:itr/polys #:itr/verts
    #:triangulate-edge-set
    #:plane-slice!
    #:plane-bisect! ; TODO
    #:plane-sym!
    #:$shape/box #:$shape/rect))

(defpackage #:gmsh/io
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn)
  (:export #:export-data #:import-data #:obj/save-mtl #:obj/load-model #:obj/load #:obj/save))

(defpackage #:gmsh/bvh
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:doc #:inline? #:wrn)
  (:export #:bvh #:@mat #:@norm #:@poly
           #:int/raycast #:int/simple-raycast
           #:simd4/simple-raycast #:simd4/raycast
           #:gpu/pack-bvh))

(defpackage #:gmsh/gl
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn #:doc)
  (:export
    #:sdl-info #:bind-buffer-object #:init-viewport #:window-context #:show-diag
    #:make-buftex #:init-controller #:make-render
    #:compile-shader #:make-program #:make-program*
    #:to-gl-array #:set-uniform-f #:set-uniform-i #:set-uniform-mat-4f #:tick))

(defpackage #:gmsh/scene
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn #:doc)
  (:export #:scene
    #:scene/make #:scene/load #:scene/save #:canv/save #:scene/new-canv
    #:update-axis #:update-view #:split-edges
    #:gpu/do-pack-bvh #:scale-to!
    #:@msh #:@cam #:@canv #:@program #:@pm #:@vm #:@size
    #:scene-matfx #:scene-matmap
    #:getmat #:setmat))

(defpackage #:gmsh/cam
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn #:doc)
  (:import-from #:auxin #:with-struct)
  (:export #:cam #:export-data #:import-data #:make
           #:@pos #:@vpn #:@up #:@u #:@v #:vm #:pm
           #:update! #:nav/around! #:nav/trans! #:nav/s! #:nav/near! #:nav/axis!
           #:set-nav-mode #:set-proj-mode #:scale-from-to!))

(defpackage #:gmsh/xrend
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn #:doc)
  (:export #:xrend #:init #:iter-timer #:get-info-fx))

(defpackage #:gvox ; TODO: incomplete
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn #:doc)
  (:export #:get-mesh #:make #:setvoxel))

