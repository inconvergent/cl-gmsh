## `gmsh/scene:@cam`
```
 ; GMSH/SCENE:@CAM
 ;   [symbol]
 ; 
 ; @CAM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Documentation:
 ;     get cam.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:@canv`
```
 ; GMSH/SCENE:@CANV
 ;   [symbol]
 ; 
 ; @CANV names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get canv
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:@msh`
```
 ; GMSH/SCENE:@MSH
 ;   [symbol]
 ; 
 ; @MSH names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES GMSH:GMSH &OPTIONAL))
 ;   Documentation:
 ;     get msh.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:@pm`
```
 ; GMSH/SCENE:@PM
 ;   [symbol]
 ; 
 ; @PM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) (VALUES VECTOR &OPTIONAL))
 ;   Documentation:
 ;     get pm.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:@program`
```
 ; GMSH/SCENE:@PROGRAM
 ;   [symbol]
 ; 
 ; @PROGRAM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES (OR STRING KEYWORD) &OPTIONAL))
 ;   Documentation:
 ;     get program.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:@size`
```
 ; GMSH/SCENE:@SIZE
 ;   [symbol]
 ; 
 ; @SIZE names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) (VALUES FIXNUM &OPTIONAL))
 ;   Documentation:
 ;     get program.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:@vm`
```
 ; GMSH/SCENE:@VM
 ;   [symbol]
 ; 
 ; @VM names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE) (VALUES VECTOR &OPTIONAL))
 ;   Documentation:
 ;     get vm.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

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
 ;   Lambda-list: (SC &OPTIONAL (NUM 32) (NUM-BUCKETS 31))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE &OPTIONAL (UNSIGNED-BYTE 32)
 ;                   (UNSIGNED-BYTE 32))
 ;                  *)
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:scale-to!`
```
:missing:

 ; GMSH/SCENE:SCALE-TO!
 ;   [symbol]
 ; 
 ; SCALE-TO! names a compiled function:
 ;   Lambda-list: (SC TO &OPTIONAL (FROM (@SIZE SC)))
 ;   Derived type: (FUNCTION (T T &OPTIONAL T)
 ;                  (VALUES GMSH/SCENE:SCENE &OPTIONAL))
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
 ;     GMSH/SCENE::PROGRAM
 ;       Type: (OR KEYWORD STRING)
 ;       Initform: :STD
 ;     GMSH/SCENE::MSH
 ;       Type: GMSH:GMSH
 ;       Initform: NIL
 ;     GMSH/SCENE::CAM
 ;       Type: GMSH/CAM:CAM
 ;       Initform: NIL
 ;     GMSH/SCENE::AXIS-STATE
 ;       Type: VEQ:FVEC
 ;       Initform: #(0.0 0.0 0.0 0.0 0.0 0.0)
 ;     GMSH/SCENE::SIZE
 ;       Type: FIXNUM
 ;       Initform: 1000
 ;     GMSH/SCENE::CANV
 ;       Type: T
 ;       Initform: NIL
 ;     GMSH/SCENE::MATFX
 ;       Type: FUNCTION
 ;       Initform: #'IDENTITY
 ;     GMSH/SCENE::MATMAP
 ;       Type: HASH-TABLE
 ;       Initform: (MAKE-HASH-TABLE :TEST #'EQUAL)
 ;     GMSH/SCENE::PM
 ;       Type: VECTOR
 ;       Initform: #(NIL)
 ;     GMSH/SCENE::VM
 ;       Type: VECTOR
 ;       Initform: #(NIL)
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
 ;                 (MSH (GMSH MAX-VERTS MAX-VERTS)) (CAM (MAKE)) MATFX
 ;                 (MATMAP (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION
 ;                  (&KEY (:SIZE (UNSIGNED-BYTE 32))
 ;                   (:MAX-VERTS (UNSIGNED-BYTE 32)) (:PROGRAM KEYWORD)
 ;                   (:MSH T) (:CAM GMSH/CAM:CAM) (:MATFX T)
 ;                   (:MATMAP HASH-TABLE))
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
 ;   Lambda-list: (SC FN &KEY MATFX OBJ (COLORS *MATPAR*) SRCFILE &AUX
 ;                 (MSH (@MSH SC)))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE STRING &KEY (:MATFX T)
 ;                   (:OBJ BOOLEAN) (:COLORS LIST) (:SRCFILE T))
 ;                  *)
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

## `gmsh/scene:split-edges`
```
 ; GMSH/SCENE:SPLIT-EDGES
 ;   [symbol]
 ; 
 ; SPLIT-EDGES names a compiled function:
 ;   Lambda-list: (SC LIM)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE SINGLE-FLOAT)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     split edges longer than lim. sometimes this makes faster raycasting (with bvh).
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

## `gmsh/scene:update-axis`
```
:missing:

 ; GMSH/SCENE:UPDATE-AXIS
 ;   [symbol]
 ; 
 ; UPDATE-AXIS names a compiled function:
 ;   Lambda-list: (SC AX VAL &AUX
 ;                 (VAL (* (SIGNUM VAL) (MAX 0 (- (ABS VAL) 3500)))))
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
 ;   Lambda-list: (SC &AUX (CAM (@CAM SC)))
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     update scene view.
 ;   Source file: /data/x/gmsh/src/gmsh/scene.lisp
```

