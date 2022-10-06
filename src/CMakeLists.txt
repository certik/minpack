set(SRC_F77
    covar.f   errjac.f  hybipt.f  lhesfcn.f  lmdipt.f  ocpipt.f  r1updt.f
    vecjac.f  chkder.f
    dmchar.f  fdjac1.f  hybrd1.f  lmder1.f   lmpar.f   qform.f   rwupdt.f
    dogleg.f  fdjac2.f  hybrd.f   lmder.f    lmstr1.f  qrfac.f   ssqfcn.f
    dpmpar.f  grdfcn.f  hybrj1.f  lmdif1.f   lmstr.f   qrsolv.f  ssqjac.f
    enorm.f   hesfcn.f  hybrj.f   lmdif.f    objfcn.f  r1mpyq.f  vecfcn.f

)
set(SRC
    ${SRC_F77}
    minpack.f90
)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set_source_files_properties(${SRC_F77} PROPERTIES COMPILE_OPTIONS
        "-std=legacy;-Wno-implicit-interface;-Wno-unused-dummy-argument;-Wno-compare-reals;-Wno-do-subscript")
else()
    set_source_files_properties(${SRC_F77} PROPERTIES COMPILE_OPTIONS
        "--fixed-form;--implicit-typing;--allow-implicit-interface")
endif()
add_library(minpack ${SRC})
install(TARGETS minpack
            RUNTIME DESTINATION bin
            ARCHIVE DESTINATION lib
            LIBRARY DESTINATION lib
        )
