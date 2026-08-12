#----------------------------------------------------------------------------------------------------------------------#
# LIB-GEOMETRY source files
#----------------------------------------------------------------------------------------------------------------------#
FILE(GLOB SOURCES_LIB_GEOMETRY
  "src/geometry-main/*.f90"
  "src/geometry-builtin/*.f90"
  "src/geometry-import/*.f90"
)
#----------------------------------------------------------------------------------------------------------------------#
#
#----------------------------------------------------------------------------------------------------------------------#
# LIB-GEOMETRY LIBRARY
#----------------------------------------------------------------------------------------------------------------------#
PROJECT(LibGeometry)
ADD_LIBRARY(${PROJECT_NAME} OBJECT ${SOURCES_LIB_GEOMETRY})
#----------------------------------------------------------------------------------------------------------------------#
# LINKED LIBRARIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_GLOBALS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_CONFIGFILES_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_DATA_STRUCTURES})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_HDF5_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_NUMERICS_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_LIB_DATA_IMPORT})
#----------------------------------------------------------------------------------------------------------------------#
# INCLUDE DIRECTORIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_GLOBALS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_CONFIGFILES_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_DATA_STRUCTURES})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_HDF5_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_NUMERICS_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_LIB_DATA_IMPORT})
#----------------------------------------------------------------------------------------------------------------------#
# GLOBAL VARIABLES
#----------------------------------------------------------------------------------------------------------------------#
SET(LINKEDLIBS_LIB_GEOMETRY ${PROJECT_NAME} CACHE INTERNAL "")
SET(INCLUDE_DIRECTORIES_LIB_GEOMETRY ${PROJECT_BINARY_DIR} CACHE INTERNAL "")
#----------------------------------------------------------------------------------------------------------------------#
# TARGET PROPERTIES
#----------------------------------------------------------------------------------------------------------------------#
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES COMPILE_FLAGS ${MAIN_FLAGS_OVERALL})
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIRECTORIES_LIB_GEOMETRY})
#----------------------------------------------------------------------------------------------------------------------#
