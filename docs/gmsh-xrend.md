## `gmsh/xrend:get-info-fx`
```
:missing:

 ; GMSH/XREND:GET-INFO-FX
 ;   [symbol]
 ; 
 ; GET-INFO-FX names a compiled function:
 ;   Lambda-list: (SIZE AA)
 ;   Derived type: (FUNCTION ((UNSIGNED-BYTE 32) (UNSIGNED-BYTE 32))
 ;                  (VALUES FUNCTION &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render-utils.lisp
```

## `gmsh/xrend:init`
```
:missing:

 ; GMSH/XREND:INIT
 ;   [symbol]
 ; 
 ; INIT names a macro:
 ;   Lambda-list: (K &KEY (CONTEXT (FUNCTION XREND-WORKER-CONTEXT))
 ;                 (BINDINGS (QUOTE ((*STCK* MAKE-FAST-STACK N 2048)))))
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render-utils.lisp
```

## `gmsh/xrend:iter-timer`
```
:missing:

 ; GMSH/XREND:ITER-TIMER
 ;   [symbol]
```

## `gmsh/xrend:render`
```
 ; GMSH/XREND:RENDER
 ;   [symbol]
 ; 
 ; RENDER names a compiled function:
 ;   Lambda-list: (SC BVH &KEY (AA 1) (PAR T) (SIZE 1000) (BS 1)
 ;                 (RAYLEN 2000.0) VOL (VMULT 134.0) (VEXPT 0.3)
 ;                 (VLIM 0.01) (VREC 7)
 ;                 (VDEPTH
 ;                  (IF VOL
 ;                      1
 ;                      0)))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE GMSH/BVH:BVH &KEY
 ;                   (:AA (UNSIGNED-BYTE 32)) (:PAR BOOLEAN)
 ;                   (:SIZE (UNSIGNED-BYTE 32)) (:BS (UNSIGNED-BYTE 32))
 ;                   (:RAYLEN SINGLE-FLOAT) (:VOL BOOLEAN)
 ;                   (:VMULT SINGLE-FLOAT) (:VEXPT SINGLE-FLOAT)
 ;                   (:VLIM SINGLE-FLOAT) (:VREC (UNSIGNED-BYTE 32))
 ;                   (:VDEPTH (UNSIGNED-BYTE 32)))
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     render scene from this scene/bvh.
 ;     
 ;     use (gmsh/xrend:init 4) before calling render to initialize parallel cores:
 ;     
 ;     keywords:
 ;      - aa     : rendering fidelity. higher is slower.
 ;      - raylen : use this ray length for sampling. not volume sampling.
 ;      - vmult  : volume light brightness. [:ll material]
 ;      - vexpt  : volume light distance falloff. lower is brighter.
 ;      - vdepth : disable volume sampling after vdepth ray bounces.
 ;      - vrec   : max recursive depth in volume sampling. higher is more detailed.
 ;      - vlim   : volume sampling min recursive sampling step size.
 ;                 lower is more detailed. [0.0, 1.0]
 ;      - vol    : enable disable volumetric light. [:ll material]
 ;      - par    : use parallelism.
 ; 
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render.lisp
```

