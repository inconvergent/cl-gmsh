(defpackage #:gmsh
  (:use #:common-lisp)
  (:nicknames #:cl-gmsh)
  (:export
    #:v? #:d? #:i? #:*opt* #:*opt1* #:*eps* #:*programs* #:*matpar* #:doc
    #:*wrncnt* #:wrn #:inline?
    #:with-fast-stack #:make-fast-stack #:stack #:stack-ind
    #:gmsh #:make-bvh #:clear! #:center!
    #:get-connected-verts #:get-num-verts #:get-num-polys #:get-max-verts
    #:get-all-polys #:add-poly! #:add-vert! #:add-verts! #:add-polys!
    #:add-plane! #:add-box! #:del-poly! #:split-edge!
    #:tx! #:classify-vert-fx #:p/classify-verts
    #:norm-poly #:itr-polys
    #:get-vert #:get-verts #:get-uv #:get-uvs #:get-norm #:get-norms
    #:plane-stretch #:plane-split #:plane-sym
    #:$shape/box #:$shape/rect))

(defpackage #:gmsh/io
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn)
  (:export #:export-data #:import-data #:obj/save-mtl #:obj/load-model #:obj/load #:obj/save))

(defpackage #:gmsh/bvh
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:doc #:inline? #:wrn)
  (:export #:bvh #:get-mat #:get-norm #:get-poly
           #:int/raycast #:int/simple-raycast
           #:simd4/simple-raycast #:simd4/raycast
           #:gpu/pack-bvh))

(defpackage #:gmsh/gl
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:doc)
  (:export
    #:sdl-info #:bind-buffer-object #:init-viewport #:window-context
    #:make-buftex #:init-controller
    #:compile-shader #:make-program #:make-program*
    #:to-gl-array #:set-uniform-f #:set-uniform-i #:set-uniform-mat-4f #:tick))

(defpackage #:gmsh/scene
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn)
  (:export #:scene
    #:get-s #:set-s #:get-pm #:get-vm
    #:scene/make #:scene/load #:scene/save #:canv/save #:scene/new-canv
    #:update-axis #:update-view #:make-vm #:make-pm
    #:gpu/do-pack-bvh
    #:scene-msh #:scene-look #:scene-proj
    #:scene-program
    #:scene-matfx #:scene-matmap
    #:scene-canv
    #:getmat #:setmat))

(defpackage #:gmsh/cam
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt*)
  (:import-from #:auxin #:with-struct)
  (:export #:cam #:export-data #:import-data #:make #:pan-cam #:pan-xy
           #:cam-u #:cam-v #:cam-s #:cam-up #:cam-pos #:cam-vpn
           #:rotate #:update #:zoom
           #:vm #:pm
           #:@pos #:@vpn #:@xy #:@up #:@s))

(defpackage #:gmsh/xrend
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:doc #:wrn)
  (:export #:xrend #:init #:iter-timer #:get-info-fx))

(defpackage #:gvox
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:doc #:wrn)
  (:export #:get-mesh #:make #:setvoxel))

; TODO: helpers for material, scene, in sep package?
; (defpackage #:bgl
;   (:use #:common-lisp)
;   (:export
;     #:load-shader #:gl-init #:build-shader #:make-program
;     #:sdl-info #:debug-log #:render))

