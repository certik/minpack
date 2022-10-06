set(SRC
    covar.f   errjac.f  hybipt.f  lhesfcn.f  lmdipt.f  ocpipt.f  r1updt.f
    vecjac.f  chkder.f
    dmchar.f  fdjac1.f  hybrd1.f  lmder1.f   lmpar.f   qform.f   rwupdt.f
    dogleg.f  fdjac2.f  hybrd.f   lmder.f    lmstr1.f  qrfac.f   ssqfcn.f
    dpmpar.f  grdfcn.f  hybrj1.f  lmdif1.f   lmstr.f   qrsolv.f  ssqjac.f
    enorm.f   hesfcn.f  hybrj.f   lmdif.f    objfcn.f  r1mpyq.f  vecfcn.f

    minpack.f90
)

if ($ENV{FC} STREQUAL "gfortran")
    add_definitions(-std=legacy -Wno-implicit-interface)
else()
    add_definitions(--fixed-form --implicit-typing --allow-implicit-interface)
endif()
add_library(minpack ${SRC})
install(TARGETS minpack
            RUNTIME DESTINATION bin
            ARCHIVE DESTINATION lib
            LIBRARY DESTINATION lib
        )
