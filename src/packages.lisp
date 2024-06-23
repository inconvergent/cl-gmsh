(defpackage #:gmsh
  (:use #:common-lisp)
  (:nicknames #:cl-gmsh)
  (:export
    #:v? #:d? #:i? #:*opt* #:*opt1* #:*eps* #:*programs* #:*matpar* #:blurb
    #:*wrncnt* #:wrn #:inline?
    #:with-fast-stack #:make-fast-stack #:stack #:stack-ind
    #:gmsh #:make-bvh #:clear! #:center!
    #:get-connected-verts #:get-num-verts #:get-num-polys #:get-max-verts
    #:get-all-polys #:add-poly! #:add-vert! #:add-verts! #:add-polys!
    #:add-plane! #:add-box! #:del-poly!
    #:tx! #:classify-vert-fx #:p/classify-verts
    #:norm-poly #:itr-polys
    #:get-vert #:get-verts #:get-uv #:get-uvs #:get-norm #:get-norms
    #:plane-stretch #:plane-split #:plane-sym
    #:$shape/box #:$shape/rect))

(defpackage #:gmsh/io
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn)
  (:export #:mexport #:mimport #:obj/mat-save #:obj/load-model #:obj/load #:obj/save))

(defpackage #:gmsh/bvh
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:blurb #:inline? #:wrn)
  (:export #:bvh #:get-mat #:get-norm #:get-poly
           #:int/raycast #:int/simple-raycast
           #:simd4/simple-raycast #:simd4/raycast
           #:gpu/pack-bvh))

(defpackage #:gmsh/scene
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:wrn)
  (:export #:scene
    #:get-s #:set-s #:get-pm #:get-vm
    #:scene/make #:scene/load #:scene/save #:canv/save #:scene/new-canv
    #:update-axis #:update-view #:make-vm #:make-pm
    #:gpu/do-pack-bvh
    #:scene-msh #:scene-proj #:scene-program #:scene-canv
    #:scene-matfx #:scene-matmap #:getmat #:setmat))

(defpackage #:gmsh/xrend ; gmsh/xrend?
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:blurb #:wrn)
  (:export #:render #:init #:iter-timer #:get-info-fx))

(defpackage #:gmsh/gl
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:blurb)
  (:export
    #:sdl-info #:bind-buffer-object #:init-viewport #:window-context
    #:make-buftex #:init-controller
    #:compile-shader #:make-program #:make-program*
    #:to-gl-array #:set-uniform-f #:set-uniform-i #:set-uniform-mat-4f #:tick))

(defpackage #:gvox
  (:use #:common-lisp)
  (:import-from #:gmsh #:*opt* #:*opt1* #:*eps* #:blurb #:wrn)
  (:export #:get-mesh #:make #:setvoxel))

; TODO: helpers for material, scene, in sep package?
; (defpackage #:bgl
;   (:use #:common-lisp)
;   (:export
;     #:load-shader #:gl-init #:build-shader #:make-program
;     #:sdl-info #:debug-log #:render))

