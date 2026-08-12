#----------------------------------------------------------------------------------------------------------------------#
# MESH-ADAPTER source files
#----------------------------------------------------------------------------------------------------------------------#
FILE(GLOB SOURCES_LIB_MESH
  "src/mesh-main/*.f90"
  "src/mesh-builtin/*.f90"
  "src/mesh-elementslist/*.f90"
  "src/mesh-export/*.f90"
  "src/mesh-import/*.f90"
  "src/mesh-geometry/*.f90"
  "src/mesh-refinement/*.f90"
)
#----------------------------------------------------------------------------------------------------------------------#
#
#----------------------------------------------------------------------------------------------------------------------#
# MESH-ADAPTER LIBRARY
#----------------------------------------------------------------------------------------------------------------------#
PROJECT(LibMesh)
ADD_LIBRARY(${PROJECT_NAME} OBJECT ${SOURCES_LIB_MESH})
#----------------------------------------------------------------------------------------------------------------------#
# LINKED LIBRARIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_GLOBALS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_CONFIGFILES_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_DATA_STRUCTURES})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_HDF5_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_NUMERICS_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_LIB_GEOMETRY})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_LIB_DATA_EXPORT})
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
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_LIB_GEOMETRY})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_LIB_DATA_EXPORT})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_LIB_DATA_IMPORT})
#----------------------------------------------------------------------------------------------------------------------#
# GLOBAL VARIABLES
#----------------------------------------------------------------------------------------------------------------------#
SET(LINKEDLIBS_LIB_MESH ${PROJECT_NAME} CACHE INTERNAL "")
SET(INCLUDE_DIRECTORIES_LIB_MESH ${PROJECT_BINARY_DIR} CACHE INTERNAL "")
#----------------------------------------------------------------------------------------------------------------------#
# TARGET PROPERTIES
#----------------------------------------------------------------------------------------------------------------------#
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES COMPILE_FLAGS ${MAIN_FLAGS_OVERALL})
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIRECTORIES_LIB_MESH})
#----------------------------------------------------------------------------------------------------------------------#
