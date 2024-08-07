## `gmsh/io:export-data`
```
 ; GMSH/IO:EXPORT-DATA
 ;   [symbol]
 ; 
 ; EXPORT-DATA names a compiled function:
 ;   Lambda-list: (MSH &KEY META
 ;                 (MATFX (LAMBDA (P) (DECLARE (IGNORE P)) (QUOTE (C X)))))
 ;   Derived type: (FUNCTION
 ;                  (GMSH:GMSH &KEY (:META LIST) (:MATFX FUNCTION))
 ;                  (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     serialize gmsh. see gmsh/io:import-data
 ;   Source file: /data/x/gmsh/src/gmsh/io.lisp
```

## `gmsh/io:import-data`
```
 ; GMSH/IO:IMPORT-DATA
 ;   [symbol]
 ; 
 ; IMPORT-DATA names a compiled function:
 ;   Lambda-list: (O &KEY MATFX MAX-VERTS)
 ;   Derived type: (FUNCTION (LIST &KEY (:MATFX T) (:MAX-VERTS T))
 ;                  (VALUES T LIST &OPTIONAL))
 ;   Documentation:
 ;     deserialize gmsh. matfx should accept two arguments, polygon
 ;     and a list of (color material). see gmsh/io:export-data
 ;   Source file: /data/x/gmsh/src/gmsh/io.lisp
```

## `gmsh/io:obj/load`
```
 ; GMSH/IO:OBJ/LOAD
 ;   [symbol]
 ; 
 ; OBJ/LOAD names a compiled function:
 ;   Lambda-list: (FN &KEY (MAX-VERTS 100000)
 ;                 (MSH (GMSH MAX-VERTS MAX-VERTS)) IGN (SILENT T) &AUX
 ;                 (FN (ENSURE-FILENAME FN .obj T)))
 ;   Derived type: (FUNCTION
 ;                  (STRING &KEY (:MAX-VERTS (UNSIGNED-BYTE 32)) (:MSH T)
 ;                          (:IGN LIST) (:SILENT T))
 ;                  (VALUES GMSH:GMSH CONS &OPTIONAL))
 ;   Documentation:
 ;     load this obj file. see: obj/load-model. see obj/load-model, obj/save
 ;   Source file: /data/x/gmsh/src/gmsh/io.lisp
```

## `gmsh/io:obj/load-model`
```
 ; GMSH/IO:OBJ/LOAD-MODEL
 ;   [symbol]
 ; 
 ; OBJ/LOAD-MODEL names a compiled function:
 ;   Lambda-list: (FN &KEY (MAX-VERTS 100000)
 ;                 (MSH (GMSH MAX-VERTS MAX-VERTS)) (CENTER NIL)
 ;                 (S (F3$VAL 1.0)) (XY (F3$ZERO)) &AUX (FN (OBJ/FN FN)))
 ;   Derived type: (FUNCTION
 ;                  ((OR STRING KEYWORD) &KEY
 ;                   (:MAX-VERTS (UNSIGNED-BYTE 32)) (:MSH GMSH:GMSH)
 ;                   (:CENTER T) (:S (SIMPLE-ARRAY SINGLE-FLOAT))
 ;                   (:XY (SIMPLE-ARRAY SINGLE-FLOAT)))
 ;                  (VALUES T LIST &OPTIONAL))
 ;   Documentation:
 ;     load this obj file. optional shift, scale, center. see obj/load, obj/save.
 ;   Source file: /data/x/gmsh/src/gmsh/io.lisp
```

## `gmsh/io:obj/save`
```
 ; GMSH/IO:OBJ/SAVE
 ;   [symbol]
 ; 
 ; OBJ/SAVE names a compiled function:
 ;   Lambda-list: (MSH FN &KEY
 ;                 (MATFX
 ;                  (LAMBDA (P) (DECLARE (IGNORABLE P)) (QUOTE (C X))))
 ;                 (PROPFX (LAMBDA (MC) (DECLARE (IGNORABLE MC)) NIL))
 ;                 &AUX (FN (ENSURE-FILENAME FN .obj T)) (VERT-OFFSET 0))
 ;   Derived type: (FUNCTION
 ;                  (GMSH:GMSH STRING &KEY (:MATFX FUNCTION)
 ;                             (:PROPFX FUNCTION))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     export obj file. see: obj/load, obj/load-model, obj/save-mtl
 ;   Source file: /data/x/gmsh/src/gmsh/io.lisp
```

## `gmsh/io:obj/save-mtl`
```
 ; GMSH/IO:OBJ/SAVE-MTL
 ;   [symbol]
 ; 
 ; OBJ/SAVE-MTL names a compiled function:
 ;   Lambda-list: (MSH FN MATS &KEY (COLORS *MATPAR*) &AUX
 ;                 (FN (ENSURE-FILENAME FN .mtl T)))
 ;   Derived type: (FUNCTION (T STRING T &KEY (:COLORS T))
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     export mtl file. see: obj/save.
 ;   Source file: /data/x/gmsh/src/gmsh/io.lisp
```

