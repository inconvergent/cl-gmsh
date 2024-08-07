## `gmsh:$shape/box`
```
 ; GMSH:$SHAPE/BOX
 ;   [symbol]
 ; 
 ; $SHAPE/BOX names a compiled function:
 ;   Lambda-list: (XY/X-182 XY/Y-183 XY/Z-184 U/X-185 U/Y-186 U/Z-187
 ;                 V/X-188 V/Y-189 V/Z-190 W/X-191 W/Y-192 W/Z-193)
 ;   Derived type: (FUNCTION
 ;                  (SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                   SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                   SINGLE-FLOAT T T T)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (24)) &OPTIONAL))
 ;   Documentation:
 ;     box centered around xy offset by the three (size) vectors: u v w
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:$shape/rect`
```
 ; GMSH:$SHAPE/RECT
 ;   [symbol]
 ; 
 ; $SHAPE/RECT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %$SHAPE/RECT
 ;     ARGS: ((VA 3 XY U V))
 ;     DOCSTRING: rectangle centered around xy offset by the two (size) vectors: u v
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:*eps*`
```
:missing:

 ; GMSH:*EPS*
 ;   [symbol]
 ; 
 ; *EPS* names a special variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 5.960465e-8
```

## `gmsh:*matpar*`
```
:missing:

 ; GMSH:*MATPAR*
 ;   [symbol]
 ; 
 ; *MATPAR* names a special variable:
 ;   Declared type: LIST
 ;   Value: ((:X 0.7 0.4 0.3 . #1=(1.0))
 ;           (:C 0.0 . #2=(1.0 . #3=(1.0 . #1#)))
 ;           (:M 1.0 . #4=(0.0 . #3#))
 ;           (:Y 1.0 . #5=(1.0 . #6=(0.0 . #1#)))
 ;           (:R 1.0 . #7=(0.0 . #6#)) (:G 0.0 . #5#) (:B 0.0 . #4#)
 ;           (:K 0.0 . #7#) (:W 1.0 . #2#))
```

## `gmsh:*opt*`
```
:missing:

 ; GMSH:*OPT*
 ;   [symbol]
 ; 
 ; *OPT* names a special variable:
 ;   Value: (OPTIMIZE (SAFETY . #1=(3)) (SPEED . #1#) (DEBUG . #1#)
 ;           (SPACE 0))
```

## `gmsh:*opt1*`
```
:missing:

 ; GMSH:*OPT1*
 ;   [symbol]
 ; 
 ; *OPT1* names a special variable:
 ;   Value: (OPTIMIZE (SAFETY . #1=(3)) (SPEED . #1#) (DEBUG . #1#)
 ;           (SPACE 0))
```

## `gmsh:*programs*`
```
:missing:

 ; GMSH:*PROGRAMS*
 ;   [symbol]
 ; 
 ; *PROGRAMS* names a special variable:
 ;   Declared type: VECTOR
 ;   Value: #(:STD :FLAT :VOL)
```

## `gmsh:*wrncnt*`
```
:missing:

 ; GMSH:*WRNCNT*
 ;   [symbol]
 ; 
 ; *WRNCNT* names a special variable:
 ;   Declared type: HASH-TABLE
 ;   Value: #<HASH-TABLE :TEST EQL :COUNT 0 {10C545DA13}>
```

## `gmsh:@all-polys`
```
 ; GMSH:@ALL-POLYS
 ;   [symbol]
 ; 
 ; @ALL-POLYS names a compiled function:
 ;   Lambda-list: (MSH)
 ;   Derived type: (FUNCTION (GMSH:GMSH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     get list of all polys.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@connected-verts`
```
 ; GMSH:@CONNECTED-VERTS
 ;   [symbol]
 ; 
 ; @CONNECTED-VERTS names a compiled function:
 ;   Lambda-list: (MSH &AUX (RES (MAKE-HASH-TABLE TEST (FUNCTION EQL))))
 ;   Derived type: (FUNCTION (GMSH:GMSH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return all connected verts.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@edge-polys`
```
 ; GMSH:@EDGE-POLYS
 ;   [symbol]
 ; 
 ; @EDGE-POLYS names a compiled function:
 ;   Lambda-list: (MSH E)
 ;   Derived type: (FUNCTION (GMSH:GMSH T) (VALUES T BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     get polys of edge.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@norm`
```
 ; GMSH:@NORM
 ;   [symbol]
 ; 
 ; @NORM names a compiled function:
 ;   Lambda-list: (MSH I)
 ;   Derived type: (FUNCTION (GMSH:GMSH (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get normal vector (3d).
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@norms`
```
 ; GMSH:@NORMS
 ;   [symbol]
 ; 
 ; @NORMS names a compiled function:
 ;   Lambda-list: (MSH INDS)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     get normals as veq:fvec (3d).
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@pnum`
```
 ; GMSH:@PNUM
 ;   [symbol]
 ; 
 ; @PNUM names a compiled function:
 ;   Lambda-list: (MSH)
 ;   Derived type: (FUNCTION (GMSH:GMSH)
 ;                  (VALUES (UNSIGNED-BYTE 44) &OPTIONAL))
 ;   Documentation:
 ;     get number of polys.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@poly`
```
 ; GMSH:@POLY
 ;   [symbol]
 ; 
 ; @POLY names a compiled function:
 ;   Lambda-list: (MSH P)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST) (VALUES T BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     return poly if it exists; or nil
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@poly-edges`
```
 ; GMSH:@POLY-EDGES
 ;   [symbol]
 ; 
 ; @POLY-EDGES names a compiled function:
 ;   Lambda-list: (MSH P)
 ;   Derived type: (FUNCTION (GMSH:GMSH T) (VALUES T BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     get edges of poly.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@uv`
```
 ; GMSH:@UV
 ;   [symbol]
 ; 
 ; @UV names a compiled function:
 ;   Lambda-list: (MSH I)
 ;   Derived type: (FUNCTION (GMSH:GMSH (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get uv pos (2d).
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@uvs`
```
 ; GMSH:@UVS
 ;   [symbol]
 ; 
 ; @UVS names a compiled function:
 ;   Lambda-list: (MSH INDS)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     get uvs as veq:fvec (2d).
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@vert`
```
 ; GMSH:@VERT
 ;   [symbol]
 ; 
 ; @VERT names a compiled function:
 ;   Lambda-list: (MSH I)
 ;   Derived type: (FUNCTION (GMSH:GMSH (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get vert 3d pos (3d).
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@verts`
```
 ; GMSH:@VERTS
 ;   [symbol]
 ; 
 ; @VERTS names a compiled function:
 ;   Lambda-list: (MSH INDS)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     get verts as veq:fvec (3d).
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@vmax`
```
 ; GMSH:@VMAX
 ;   [symbol]
 ; 
 ; @VMAX names a compiled function:
 ;   Lambda-list: (MSH)
 ;   Derived type: (FUNCTION (GMSH:GMSH)
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     get max number of verts.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:@vnum`
```
 ; GMSH:@VNUM
 ;   [symbol]
 ; 
 ; @VNUM names a compiled function:
 ;   Lambda-list: (MSH)
 ;   Derived type: (FUNCTION (GMSH:GMSH)
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     get number of verts
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:add-box!`
```
 ; GMSH:ADD-BOX!
 ;   [symbol]
 ; 
 ; ADD-BOX! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %ADD-BOX!
 ;     ARGS: (MSH (VA 3 XY) &OPTIONAL (SX 1.0) (SY SX) (SZ SY))
 ;     DOCSTRING: add box centered at xy, with size (sx sy sz). returns polys
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:add-plane!`
```
 ; GMSH:ADD-PLANE!
 ;   [symbol]
 ; 
 ; ADD-PLANE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %ADD-PLANE!
 ;     ARGS: (MSH (VA 3 XY) &OPTIONAL (SX 1.0) (SY SX))
 ;     DOCSTRING: add plane centered at xy with size (sx sy). retunrns list with 2 tris
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:add-poly!`
```
 ; GMSH:ADD-POLY!
 ;   [symbol]
 ; 
 ; ADD-POLY! names a compiled function:
 ;   Lambda-list: (MSH POLY &OPTIONAL VT)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST &OPTIONAL LIST)
 ;                  (VALUES LIST (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     add poly to msh. returns poly if it was created; or nil.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:add-polys!`
```
 ; GMSH:ADD-POLYS!
 ;   [symbol]
 ; 
 ; ADD-POLYS! names a compiled function:
 ;   Lambda-list: (MSH PP)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     add these polygons.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:add-vert!`
```
 ; GMSH:ADD-VERT!
 ;   [symbol]
 ; 
 ; ADD-VERT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %ADD-VERT!
 ;     ARGS: (MSH (VA 3 X))
 ;     DOCSTRING: add vert.
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:add-verts!`
```
 ; GMSH:ADD-VERTS!
 ;   [symbol]
 ; 
 ; ADD-VERTS! names a compiled function:
 ;   Lambda-list: (MSH V)
 ;   Derived type: (FUNCTION (T T) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     add these verts.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:center!`
```
 ; GMSH:CENTER!
 ;   [symbol]
 ; 
 ; CENTER! names a compiled function:
 ;   Lambda-list: (MSH &KEY MAX-SIDE (CONNECTED T))
 ;   Derived type: (FUNCTION
 ;                  (GMSH:GMSH &KEY (:MAX-SIDE T) (:CONNECTED BOOLEAN))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     center gmsh. optionally scale max side t, only affect connected verts.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:clear!`
```
 ; GMSH:CLEAR!
 ;   [symbol]
 ; 
 ; CLEAR! names a compiled function:
 ;   Lambda-list: (GMSH)
 ;   Derived type: (FUNCTION (GMSH:GMSH) (VALUES GMSH:GMSH &OPTIONAL))
 ;   Documentation:
 ;     clear gmsh.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:d?`
```
:missing:

 ; GMSH:D?
 ;   [symbol]
 ; 
 ; D? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/utils.lisp
```

## `gmsh:del-poly!`
```
 ; GMSH:DEL-POLY!
 ;   [symbol]
 ; 
 ; DEL-POLY! names a compiled function:
 ;   Lambda-list: (MSH POLY)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     delete this poly.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:doc`
```
 ; GMSH:DOC
 ;   [symbol]
 ; 
 ; DOC names a macro:
 ;   Lambda-list: (&REST BODY)
 ;   Documentation:
 ;     ignores body. returns nil.
 ;   Source file: /data/x/gmsh/src/utils.lisp
```

## `gmsh:gmsh`
```
 ; GMSH:GMSH
 ;   [symbol]
 ; 
 ; GMSH names a compiled function:
 ;   Lambda-list: (&KEY (MAX-VERTS 100000) PROPS)
 ;   Derived type: (FUNCTION
 ;                  (&KEY (:MAX-VERTS (UNSIGNED-BYTE 32)) (:PROPS LIST))
 ;                  (VALUES GMSH:GMSH &OPTIONAL))
 ;   Documentation:
 ;     make gmsh object.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
 ; 
 ; GMSH names the structure-class #<STRUCTURE-CLASS GMSH:GMSH>:
 ;   Documentation:
 ;     simple triangular mesh object with mesh manipulation and texture mapping.
 ;   Class precedence-list: GMSH, STRUCTURE-OBJECT, SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Slots:
 ;     OBJ-PROPS
 ;       Type: T
 ;       Initform: (LIST)
 ;     POLYS
 ;       Type: HASH-TABLE
 ;       Initform: (MAKE-HT #'EQUAL 5000)
 ;     EDGES->POLY
 ;       Type: HASH-TABLE
 ;       Initform: (MAKE-HT #'EQUAL 5000)
 ;     NUM-UVS
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     NUM-VERTS
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     MAX-VERTS
 ;       Type: VEQ:PN
 ;       Initform: 100000
 ;     VERTS
 ;       Type: T
 ;       Initform: NIL
 ;     NORMS
 ;       Type: T
 ;       Initform: NIL
 ;     UV
 ;       Type: T
 ;       Initform: NIL
 ;     VN
 ;       Type: T
 ;       Initform: NIL
 ;     VT
 ;       Type: T
 ;       Initform: NIL
```

## `gmsh:i?`
```
:missing:

 ; GMSH:I?
 ;   [symbol]
 ; 
 ; I? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/utils.lisp
```

## `gmsh:inline?`
```
:missing:

 ; GMSH:INLINE?
 ;   [symbol]
```

## `gmsh:itr/polys`
```
 ; GMSH:ITR/POLYS
 ;   [symbol]
 ; 
 ; ITR/POLYS names a macro:
 ;   Lambda-list: ((MSH P &KEY COLLECT) &BODY BODY)
 ;   Documentation:
 ;     iterate all polygons as p. optionally collect body.
 ;   Source file: /data/x/gmsh/src/macros.lisp
```

## `gmsh:itr/verts`
```
 ; GMSH:ITR/VERTS
 ;   [symbol]
 ; 
 ; ITR/VERTS names a macro:
 ;   Lambda-list: ((MSH I X Y Z &KEY COLLECT) &BODY BODY)
 ;   Documentation:
 ;     iterate all vertices as index and pos. optionally collect body.
 ;   Source file: /data/x/gmsh/src/macros.lisp
```

## `gmsh:make-bvh`
```
 ; GMSH:MAKE-BVH
 ;   [symbol]
 ; 
 ; MAKE-BVH names a compiled function:
 ;   Lambda-list: (MSH &REST REST)
 ;   Derived type: (FUNCTION (GMSH:GMSH &REST T) *)
 ;   Documentation:
 ;     
 ;     construct bvh for rendering.
 ;     
 ;     keywords:
 ;      - num         : smallest number of polys in a node.
 ;      - mode        : acceleration structure.
 ;      - num-buckets : SAH estimation buckets.
 ; 
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:make-fast-stack`
```
:missing:

 ; GMSH:MAKE-FAST-STACK
 ;   [symbol]
```

## `gmsh:msk/set!`
```
:missing:

 ; GMSH:MSK/SET!
 ;   [symbol]
```

## `gmsh:msk/tx!`
```
 ; GMSH:MSK/TX!
 ;   [symbol]
 ; 
 ; MSK/TX! names a macro:
 ;   Lambda-list: (MSH POS MSK &BODY BODY)
 ;   Documentation:
 ;     apply this transform to verts where msk is not 0
 ;   Source file: /data/x/gmsh/src/macros.lisp
```

## `gmsh:msk/v`
```
 ; GMSH:MSK/V
 ;   [symbol]
 ; 
 ; MSK/V names a macro:
 ;   Lambda-list: (MSH POS &BODY BODY)
 ;   Documentation:
 ;     return veq:pvec mask
 ;   Source file: /data/x/gmsh/src/macros.lisp
```

## `gmsh:norm-poly`
```
 ; GMSH:NORM-POLY
 ;   [symbol]
 ; 
 ; NORM-POLY names a compiled function:
 ;   Lambda-list: (POLY &AUX (I (FIND-MIN-IND POLY)))
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES LIST (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     normalize polygon. smallest ind first. maintains order.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:plane-bisect!`
```
:missing:

 ; GMSH:PLANE-BISECT!
 ;   [symbol]
```

## `gmsh:plane-slice!`
```
 ; GMSH:PLANE-SLICE!
 ;   [symbol]
 ; 
 ; PLANE-SLICE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %PLANE-SLICE!
 ;     ARGS: (MSH (VA 3 PT NORM) &KEY MATFX &AUX (G (GRPH)) (EDGE-CACHE (MAKE-HT))
 ;            (LERPS (MAKE-HT)) (NEW-VERTS (LIST)) (NEW-EDGE-SET (LIST)))
 ;     DOCSTRING: slice mesh with plane.
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/isect.lisp
```

## `gmsh:plane-sym!`
```
 ; GMSH:PLANE-SYM!
 ;   [symbol]
 ; 
 ; PLANE-SYM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %PLANE-SYM!
 ;     ARGS: (MSH (VA 3 PT NORM) &KEY MATFX &AUX
 ;            (SIDE-IDX (MAKE-HASH-TABLE TEST #'EQ)))
 ;     DOCSTRING: cut and make mesh symmetric around plane.
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/isect.lisp
```

## `gmsh:split-edge!`
```
 ; GMSH:SPLIT-EDGE!
 ;   [symbol]
 ; 
 ; SPLIT-EDGE! names a compiled function:
 ;   Lambda-list: (MSH E &KEY MATFX)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST &KEY (:MATFX T))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     split this edge
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:stack`
```
:missing:

 ; GMSH:STACK
 ;   [symbol]
```

## `gmsh:stack-ind`
```
:missing:

 ; GMSH:STACK-IND
 ;   [symbol]
```

## `gmsh:triangulate-edge-set`
```
 ; GMSH:TRIANGULATE-EDGE-SET
 ;   [symbol]
 ; 
 ; TRIANGULATE-EDGE-SET names a compiled function:
 ;   Lambda-list: (MSH EDGE-SET PFX)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST FUNCTION)
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     triangulate the hull defined by edge set, using the projection provided
 ;     by pfx where pfx: R3 -> R2.
 ;   Source file: /data/x/gmsh/src/gmsh/isect.lisp
```

## `gmsh:tx!`
```
 ; GMSH:TX!
 ;   [symbol]
 ; 
 ; TX! names a compiled function:
 ;   Lambda-list: (MSH INDS FX)
 ;   Derived type: (FUNCTION (GMSH:GMSH LIST FUNCTION)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     apply this transform to verts.
 ;   Source file: /data/x/gmsh/src/gmsh/gmsh.lisp
```

## `gmsh:v?`
```
:missing:

 ; GMSH:V?
 ;   [symbol]
 ; 
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T) &AUX
 ;                 (V
 ;                  (SLOT-VALUE (FIND-SYSTEM (QUOTE GMSH))
 ;                              (QUOTE VERSION))))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/utils.lisp
```

## `gmsh:with-fast-stack`
```
:missing:

 ; GMSH:WITH-FAST-STACK
 ;   [symbol]
```

## `gmsh:wrn`
```
 ; GMSH:WRN
 ;   [symbol]
 ; 
 ; WRN names a macro:
 ;   Lambda-list: (K S &REST REST)
 ;   Documentation:
 ;     show limited number of warnings. counts by k. shows gmsh::*wrnlimit* warnings for each k.
 ;   Source file: /data/x/gmsh/src/config.lisp
```

