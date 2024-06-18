## `gvox:get-mesh`
```
 ; GVOX:GET-MESH
 ;   [symbol]
 ; 
 ; GET-MESH names a compiled function:
 ;   Lambda-list: (MSH VOXS &KEY W
 ;                 (FX (LAMBDA (V) (DECLARE (FF V)) (>= 0.0 V))))
 ;   Derived type: (FUNCTION
 ;                  (T GVOX::VOXELS &KEY (:W BOOLEAN) (:FX FUNCTION))
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     reconstruct mesh surounding (fx ...) == t.
 ;   Source file: /data/x/gmsh/src/voxel/voxel.lisp
```

## `gvox:make`
```
:missing:

 ; GVOX:MAKE
 ;   [symbol]
 ; 
 ; MAKE names a compiled function:
 ;   Lambda-list: (DIM)
 ;   Derived type: (FUNCTION (LIST) (VALUES GVOX::VOXELS &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/voxel/init.lisp
```

## `gvox:setvoxel`
```
:missing:

 ; GVOX:SETVOXEL
 ;   [symbol]
 ; 
 ; SETVOXEL names a compiled function:
 ;   Lambda-list: (VOXS IX IY IZ &OPTIONAL (V 1.0))
 ;   Derived type: (FUNCTION
 ;                  (GVOX::VOXELS (UNSIGNED-BYTE 32) (UNSIGNED-BYTE 32)
 ;                   (UNSIGNED-BYTE 32) &OPTIONAL SINGLE-FLOAT)
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/voxel/init.lisp
```

