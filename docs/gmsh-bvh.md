## `gmsh/bvh:@mat`
```
 ; GMSH/BVH:@MAT
 ;   [symbol]
 ; 
 ; @MAT names a compiled function:
 ;   Lambda-list: (BVH I)
 ;   Derived type: (FUNCTION (GMSH/BVH:BVH (SIGNED-BYTE 32))
 ;                  (VALUES SYMBOL SYMBOL &OPTIONAL))
 ;   Documentation:
 ;     get this material from bvh.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh.lisp
```

## `gmsh/bvh:@norm`
```
 ; GMSH/BVH:@NORM
 ;   [symbol]
 ; 
 ; @NORM names a compiled function:
 ;   Lambda-list: (BVH I)
 ;   Derived type: (FUNCTION (GMSH/BVH:BVH (SIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get this normal from bvh
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh.lisp
```

## `gmsh/bvh:@poly`
```
 ; GMSH/BVH:@POLY
 ;   [symbol]
 ; 
 ; @POLY names a compiled function:
 ;   Lambda-list: (BVH I)
 ;   Derived type: (FUNCTION (GMSH/BVH:BVH (SIGNED-BYTE 32))
 ;                  (VALUES (UNSIGNED-BYTE 32) (UNSIGNED-BYTE 32)
 ;                          (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     get this poly from bvh.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh.lisp
```

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
 ;     GMSH/BVH::SIMD-NODES
 ;       Type: VEQ:PVEC
 ;       Initform: (VEQ:P4$ZERO 0)
 ;     GMSH/BVH::SIMD-MIMA
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F4$ZERO 0)
 ;     GMSH/BVH::POLYFX
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F3$ZERO 0)
 ;     GMSH/BVH::NORMALS
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F3$ZERO 0)
 ;     GMSH/BVH::POLYS
 ;       Type: VEQ:PVEC
 ;       Initform: (VEQ:P3$ZERO 0)
 ;     GMSH/BVH::MAT
 ;       Type: VEQ:SVEC
 ;       Initform: NIL
 ;     GMSH/BVH::MIMA
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F4$ZERO 0)
 ;     GMSH/BVH::NODES
 ;       Type: GMSH/BVH::BVH-NODES
 ;       Initform: (GMSH/BVH::INIT-BVH-NODES 1)
 ;     GMSH/BVH::INT-NODES
 ;       Type: VEQ:PVEC
 ;       Initform: (VEQ:P4$ZERO 0)
 ;     GMSH/BVH::MODE
 ;       Type: KEYWORD
 ;       Initform: :BVH4-SIMD
 ;     GMSH/BVH::NUM-LEAVES
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     GMSH/BVH::NUM-NODES
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     GMSH/BVH::NUM-POLYS
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     GMSH/BVH::CNT
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     GMSH/BVH::LVL
 ;       Type: VEQ:PN
 ;       Initform: 0
 ;     TIME
 ;       Type: VEQ:FF (unboxed)
 ;       Initform: 0.0
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
 ; INT/RAYCAST names a compiled function:
 ;   Lambda-list: (BVH ORG/X-0 ORG/Y-1 ORG/Z-2 LL/X-3 LL/Y-4 LL/Z-5)
 ;   Derived type: (FUNCTION
 ;                  (GMSH/BVH:BVH SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                   SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT)
 ;                  (VALUES (INTEGER -1 477218587)
 ;                          (OR (SINGLE-FLOAT 900000.0 900000.0)
 ;                              (SINGLE-FLOAT (5.960465e-8) (1.0)))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     raycast from org along ll. returns (values i s), where
 ;       - i is the index (in the bvh) of the closest polygon, or -1,
 ;       - s is the lerp along ll.
 ;      bvh must be constructed with mode :bh2-stackless
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast.lisp
```

## `gmsh/bvh:int/simple-raycast`
```
 ; GMSH/BVH:INT/SIMPLE-RAYCAST
 ;   [symbol]
 ; 
 ; INT/SIMPLE-RAYCAST names a compiled function:
 ;   Lambda-list: (BVH ORG/X-0 ORG/Y-1 ORG/Z-2 LL/X-3 LL/Y-4 LL/Z-5)
 ;   Derived type: (FUNCTION
 ;                  (GMSH/BVH:BVH SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                   SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT)
 ;                  (VALUES
 ;                   (OR (SINGLE-FLOAT 0.0 0.0) (SINGLE-FLOAT 1.0 1.0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     raycast from org along ll. returns (values i s), where
 ;       - i is the index (in the bvh) of the closest polygon, or -1,
 ;       - s is the lerp along ll.
 ;      bvh must be constructed with mode :bvh2-stackless
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast.lisp
```

## `gmsh/bvh:simd4/raycast`
```
 ; GMSH/BVH:SIMD4/RAYCAST
 ;   [symbol]
 ; 
 ; SIMD4/RAYCAST names a compiled function:
 ;   Lambda-list: (BVH ORG/X-0 ORG/Y-1 ORG/Z-2 LL/X-3 LL/Y-4 LL/Z-5)
 ;   Derived type: (FUNCTION
 ;                  (GMSH/BVH:BVH SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                   SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT)
 ;                  (VALUES (INTEGER -1 477218588)
 ;                          (OR (SINGLE-FLOAT 900000.0 900000.0)
 ;                              (SINGLE-FLOAT (5.960465e-8) (1.0)))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
 ;     bvh must be constructed with mode :bvh4-simd.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast-simd.lisp
```

## `gmsh/bvh:simd4/simple-raycast`
```
 ; GMSH/BVH:SIMD4/SIMPLE-RAYCAST
 ;   [symbol]
 ; 
 ; SIMD4/SIMPLE-RAYCAST names a compiled function:
 ;   Lambda-list: (BVH ORG/X-0 ORG/Y-1 ORG/Z-2 LL/X-3 LL/Y-4 LL/Z-5)
 ;   Derived type: (FUNCTION
 ;                  (GMSH/BVH:BVH SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                   SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT)
 ;                  (VALUES
 ;                   (OR (SINGLE-FLOAT 0.0 0.0) (SINGLE-FLOAT 1.0 1.0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     raycast from org along ll. returns 0.0 if something is hit, 1.0 otherwise.
 ;     bvh must be constructed with mode :bvh4-simd.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/bvh-raycast-simd.lisp
```

