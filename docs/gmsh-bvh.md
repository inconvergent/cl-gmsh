## `gmsh/bvh:bvh`
```
:missing:

 ; GMSH/BVH:BVH
 ;   [symbol]
 ; 
 ; BVH names the structure-class #<STRUCTURE-CLASS GMSH/BVH:BVH>:
 ;   Documentation:
 ;     static BVH object for rendering
 ;   Class precedence-list: GMSH/BVH:BVH, STRUCTURE-OBJECT,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Slots:
 ;     TIME
 ;       Type: VEQ:FF (unboxed)
 ;       Initform: 0.0
 ;     GMSH/BVH::NODES
 ;       Type: GMSH/BVH::BVH-NODE-VEC
 ;       Initform: (GMSH/BVH::INIT-BVH-NODE-VEC 1)
 ;     GMSH/BVH::INT-NODES
 ;       Type: VEQ:PVEC
 ;       Initform: (VEQ:P4$ZERO 0)
 ;     GMSH/BVH::SIMD-NODES
 ;       Type: VEQ:IVEC
 ;       Initform: (VEQ:I4$ZERO 0)
 ;     GMSH/BVH::POLYS
 ;       Type: VEQ:PVEC
 ;       Initform: (VEQ:P3$ZERO 0)
 ;     GMSH/BVH::POLYFX
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F3$ZERO 0)
 ;     GMSH/BVH::NORMALS
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F3$ZERO 0)
 ;     GMSH/BVH::MIMA
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F4$ZERO 0)
 ;     GMSH/BVH::SIMD-MIMA
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F4$ZERO 0)
 ;     GMSH/BVH::LVL
 ;       Type: FIXNUM
 ;       Initform: 0
 ;     GMSH/BVH::MAT
 ;       Type: VEQ:SVEC
 ;       Initform: NIL
 ;     GMSH/BVH::NUM-LEAVES
 ;       Type: FIXNUM
 ;       Initform: 0
 ;     GMSH/BVH::NUM-NODES
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     GMSH/BVH::NUM-POLYS
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     GMSH/BVH::MODE
 ;       Type: KEYWORD
 ;       Initform: :BVH2
 ;     GMSH/BVH::CNT
 ;       Type: FIXNUM
 ;       Initform: 0
```

## `gmsh/bvh:get-mat`
```
 ; GMSH/BVH:GET-MAT
 ;   [symbol]
 ; 
 ; GET-MAT names a compiled function:
 ;   Lambda-list: (BVH I)
 ;   Derived type: (FUNCTION (GMSH/BVH:BVH (SIGNED-BYTE 32))
 ;                  (VALUES SYMBOL SYMBOL &OPTIONAL))
 ;   Documentation:
 ;     get this material from bvh.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh.lisp
```

## `gmsh/bvh:get-norm`
```
 ; GMSH/BVH:GET-NORM
 ;   [symbol]
 ; 
 ; GET-NORM names a compiled function:
 ;   Lambda-list: (BVH I)
 ;   Derived type: (FUNCTION (GMSH/BVH:BVH (SIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get this normal from bvh
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh.lisp
```

## `gmsh/bvh:get-poly`
```
 ; GMSH/BVH:GET-POLY
 ;   [symbol]
 ; 
 ; GET-POLY names a compiled function:
 ;   Lambda-list: (BVH I)
 ;   Derived type: (FUNCTION (GMSH/BVH:BVH (SIGNED-BYTE 32))
 ;                  (VALUES (UNSIGNED-BYTE 32) (UNSIGNED-BYTE 32)
 ;                          (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     get this poly from bvh.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh.lisp
```

## `gmsh/bvh:gpu/pack-bvh`
```
 ; GMSH/BVH:GPU/PACK-BVH
 ;   [symbol]
 ; 
 ; GPU/PACK-BVH names a compiled function:
 ;   Lambda-list: (BVH &KEY (MATS *MATS*) (MATPAR *MATPAR*))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/BVH:BVH &KEY (:MATS LIST) (:MATPAR LIST))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT . #1=((*))) T
 ;                          (SIMPLE-ARRAY SINGLE-FLOAT . #1#) T
 ;                          (SIMPLE-ARRAY (SIGNED-BYTE 32) . #1#) T
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     return packed bvh arrays for gpu rendering.
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-stack.lisp
```

## `gmsh/bvh:int/raycast`
```
 ; GMSH/BVH:INT/RAYCAST
 ;   [symbol]
 ; 
 ; INT/RAYCAST names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %INT/RAYCAST
 ;     ARGS: (BVH (VA 3 ORG LL))
 ;     DOCSTRING: raycast from org along ll. returns (values i s), where
 ;       - i is the index (in the bvh) of the closest polygon, or -1,
 ;       - s is the lerp along ll.
 ;      bvh must be constructed with mode :bh2-stackless
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast.lisp
```

## `gmsh/bvh:int/simple-raycast`
```
 ; GMSH/BVH:INT/SIMPLE-RAYCAST
 ;   [symbol]
 ; 
 ; INT/SIMPLE-RAYCAST names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %INT/SIMPLE-RAYCAST
 ;     ARGS: (BVH (VA 3 ORG LL))
 ;     DOCSTRING: raycast from org along ll. returns (values i s), where
 ;       - i is the index (in the bvh) of the closest polygon, or -1,
 ;       - s is the lerp along ll.
 ;      bvh must be constructed with mode :bvh2-stackless
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast.lisp
```

## `gmsh/bvh:simd4/raycast`
```
 ; GMSH/BVH:SIMD4/RAYCAST
 ;   [symbol]
 ; 
 ; SIMD4/RAYCAST names a macro:
 ;   Lambda-list: (BVH &REST REST)
 ;   Documentation:
 ;     raycast bvh from org alon ll using simd.
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast.lisp
```

## `gmsh/bvh:simd4/simple-raycast`
```
 ; GMSH/BVH:SIMD4/SIMPLE-RAYCAST
 ;   [symbol]
 ; 
 ; SIMD4/SIMPLE-RAYCAST names a macro:
 ;   Lambda-list: (BVH &REST REST)
 ;   Documentation:
 ;     raycast bvh from org alon ll using simd.
 ;     returns 0.0 if it hits anything; 1.0 otherwise
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast.lisp
```

