## `gmsh/scene:canv/save`
```
 ; GMSH/SCENE:CANV/SAVE
 ;   [symbol]
 ; 
 ; CANV/SAVE names a compiled function:
 ;   Lambda-list: (SC FN &KEY (GAMMA 1.0))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE STRING &KEY (:GAMMA SINGLE-FLOAT)) *)
 ;   Documentation:
 ;     save scene canvas to fn as png.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:get-pm`
```
 ; GMSH/SCENE:GET-PM
 ;   [symbol]
 ; 
 ; GET-PM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) (VALUES VECTOR &OPTIONAL))
 ;   Documentation:
 ;     current projection matrix.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:get-s`
```
 ; GMSH/SCENE:GET-S
 ;   [symbol]
 ; 
 ; GET-S names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     current scale.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:get-vm`
```
 ; GMSH/SCENE:GET-VM
 ;   [symbol]
 ; 
 ; GET-VM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) (VALUES VECTOR &OPTIONAL))
 ;   Documentation:
 ;     current view matrix.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:getmat`
```
 ; GMSH/SCENE:GETMAT
 ;   [symbol]
 ; 
 ; GETMAT names a compiled function:
 ;   Lambda-list: (SC P &OPTIONAL DEFAULT &AUX (MATMAP (SCENE-MATMAP SC))
 ;                 (RES (GETHASH P MATMAP)))
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE LIST &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get poly material
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:gpu/do-pack-bvh`
```
:missing:

 ; GMSH/SCENE:GPU/DO-PACK-BVH
 ;   [symbol]
 ; 
 ; GPU/DO-PACK-BVH names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) *)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:make-pm`
```
 ; GMSH/SCENE:MAKE-PM
 ;   [symbol]
 ; 
 ; MAKE-PM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES (SIMPLE-VECTOR 1) &OPTIONAL))
 ;   Documentation:
 ;     new projection matrix.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:make-vm`
```
 ; GMSH/SCENE:MAKE-VM
 ;   [symbol]
 ; 
 ; MAKE-VM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES (SIMPLE-VECTOR 1) &OPTIONAL))
 ;   Documentation:
 ;     new view matrix.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene`
```
:missing:

 ; GMSH/SCENE:SCENE
 ;   [symbol]
 ; 
 ; SCENE names the structure-class #<STRUCTURE-CLASS GMSH/SCENE:SCENE>:
 ;   Documentation:
 ;     unified scene object for gpu/cpu rendering. with file export/import.
 ;   Class precedence-list: GMSH/SCENE:SCENE, STRUCTURE-OBJECT,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Slots:
 ;     GMSH/SCENE::MSH
 ;       Type: GMSH:GMSH
 ;       Initform: NIL
 ;     GMSH/SCENE::PROGRAM
 ;       Type: (OR KEYWORD STRING)
 ;       Initform: :STD
 ;     GMSH/SCENE::PROJ
 ;       Type: ORTHO:ORTHO
 ;       Initform: (ORTHO:MAKE)
 ;     GMSH/SCENE::LOOK
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F3$POINT 0.0 0.0 0.0)
 ;     GMSH/SCENE::AXIS-STATE
 ;       Type: VEQ:FVEC
 ;       Initform: #(0.0 0.0 0.0 0.0 0.0 0.0 0.0)
 ;     GMSH/SCENE::SIZE
 ;       Type: FIXNUM
 ;       Initform: 1000
 ;     GMSH/SCENE::CANV
 ;       Type: T
 ;       Initform: NIL
 ;     GMSH/SCENE::LIM
 ;       Type: VEQ:FF (unboxed)
 ;       Initform: 1.0e-5
 ;     GMSH/SCENE::MATFX
 ;       Type: FUNCTION
 ;       Initform: #'IDENTITY
 ;     GMSH/SCENE::MATMAP
 ;       Type: HASH-TABLE
 ;       Initform: (MAKE-HASH-TABLE :TEST #'EQUAL)
 ;     GMSH/SCENE::PM
 ;       Type: VECTOR
 ;       Initform: #()
 ;     GMSH/SCENE::VM
 ;       Type: VECTOR
 ;       Initform: #()
```

## `gmsh/scene:scene-canv`
```
:missing:

 ; GMSH/SCENE:SCENE-CANV
 ;   [symbol]
 ; 
 ; SCENE-CANV names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) (VALUES T &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
 ; 
 ; (SETF SCENE-CANV) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION (T GMSH/SCENE:SCENE) (VALUES T &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene-matfx`
```
:missing:

 ; GMSH/SCENE:SCENE-MATFX
 ;   [symbol]
 ; 
 ; SCENE-MATFX names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES FUNCTION &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
 ; 
 ; (SETF SCENE-MATFX) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION (FUNCTION GMSH/SCENE:SCENE)
 ;                  (VALUES FUNCTION &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene-matmap`
```
:missing:

 ; GMSH/SCENE:SCENE-MATMAP
 ;   [symbol]
 ; 
 ; SCENE-MATMAP names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES HASH-TABLE &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene-msh`
```
:missing:

 ; GMSH/SCENE:SCENE-MSH
 ;   [symbol]
 ; 
 ; SCENE-MSH names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES GMSH:GMSH &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene-program`
```
:missing:

 ; GMSH/SCENE:SCENE-PROGRAM
 ;   [symbol]
 ; 
 ; SCENE-PROGRAM names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES (OR STRING KEYWORD) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
 ; 
 ; (SETF SCENE-PROGRAM) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION ((OR STRING KEYWORD) GMSH/SCENE:SCENE)
 ;                  (VALUES (OR STRING KEYWORD) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene-proj`
```
:missing:

 ; GMSH/SCENE:SCENE-PROJ
 ;   [symbol]
 ; 
 ; SCENE-PROJ names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES ORTHO:ORTHO &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene/load`
```
:missing:

 ; GMSH/SCENE:SCENE/LOAD
 ;   [symbol]
 ; 
 ; SCENE/LOAD names a compiled function:
 ;   Lambda-list: (FN &AUX (O (DAT-READ-ONE FN))
 ;                 (MATMAP (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION (STRING) *)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene/make`
```
:missing:

 ; GMSH/SCENE:SCENE/MAKE
 ;   [symbol]
 ; 
 ; SCENE/MAKE names a compiled function:
 ;   Lambda-list: (&KEY (SIZE 1000) (MAX-VERTS 2000000) (PROGRAM STD)
 ;                 (MSH (GMSH MAX-VERTS MAX-VERTS))
 ;                 (CAM (F3$POINT 401.0 400.0 101.0)) (LOOK (F3$ZERO))
 ;                 (S 1.0) (XY (F2$POINT 1000.0 1000.0))
 ;                 (PROJ (MAKE CAM CAM LOOK LOOK XY XY S S))
 ;                 (MATMAP (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))) MATFX)
 ;   Derived type: (FUNCTION
 ;                  (&KEY (:SIZE (UNSIGNED-BYTE 32))
 ;                   (:MAX-VERTS (UNSIGNED-BYTE 32)) (:PROGRAM KEYWORD)
 ;                   (:MSH T) (:CAM (SIMPLE-ARRAY SINGLE-FLOAT))
 ;                   (:LOOK (SIMPLE-ARRAY SINGLE-FLOAT)) (:S SINGLE-FLOAT)
 ;                   (:XY (SIMPLE-ARRAY SINGLE-FLOAT)) (:PROJ ORTHO:ORTHO)
 ;                   (:MATMAP HASH-TABLE) (:MATFX T))
 ;                  (VALUES GMSH/SCENE:SCENE &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene/new-canv`
```
:missing:

 ; GMSH/SCENE:SCENE/NEW-CANV
 ;   [symbol]
 ; 
 ; SCENE/NEW-CANV names a compiled function:
 ;   Lambda-list: (SC &KEY SIZE)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE &KEY (:SIZE T))
 ;                  (VALUES GMSH/SCENE:SCENE &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scene/save`
```
:missing:

 ; GMSH/SCENE:SCENE/SAVE
 ;   [symbol]
 ; 
 ; SCENE/SAVE names a compiled function:
 ;   Lambda-list: (SC FN &KEY MATFX (COLORS *MATPAR*) &AUX
 ;                 (MSH (SCENE-MSH SC)))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE STRING &KEY (:MATFX T) (:COLORS T))
 ;                  *)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:set-s`
```
 ; GMSH/SCENE:SET-S
 ;   [symbol]
 ; 
 ; SET-S names a compiled function:
 ;   Lambda-list: (SC S)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE T) *)
 ;   Documentation:
 ;     set new scale.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:setmat`
```
 ; GMSH/SCENE:SETMAT
 ;   [symbol]
 ; 
 ; SETMAT names a compiled function:
 ;   Lambda-list: (SC P M &AUX (MATMAP (SCENE-MATMAP SC)))
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE LIST LIST)
 ;                  (VALUES (OR CONS (INTEGER 1 4294967296)) &OPTIONAL))
 ;   Documentation:
 ;     set poly mat
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:update-axis`
```
:missing:

 ; GMSH/SCENE:UPDATE-AXIS
 ;   [symbol]
 ; 
 ; UPDATE-AXIS names a compiled function:
 ;   Lambda-list: (SC AX V &AUX
 ;                 (V (* (SIGNUM V) (MAX 0 (- (ABS V) 3500)))))
 ;   Derived type: (FUNCTION (T T T)
 ;                  (VALUES (SINGLE-FLOAT -1.0 1.0) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:update-view`
```
 ; GMSH/SCENE:UPDATE-VIEW
 ;   [symbol]
 ; 
 ; UPDATE-VIEW names a compiled function:
 ;   Lambda-list: (SC &AUX (LOOK (SCENE-LOOK SC)))
 ;   Derived type: (FUNCTION (T) (VALUES (SIMPLE-VECTOR 1) &OPTIONAL))
 ;   Documentation:
 ;     update scene view.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

