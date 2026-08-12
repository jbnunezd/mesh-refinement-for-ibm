#----------------------------------------------------------------------------------------------------------------------#
# MESH-REFINEMENT source files
#----------------------------------------------------------------------------------------------------------------------#
FILE(GLOB SOURCES_MAIN_MESH_REFINEMENT
  "src/*.f90"
)
#----------------------------------------------------------------------------------------------------------------------#
#
#----------------------------------------------------------------------------------------------------------------------#
# MESH-REFINEMENT LIBRARY
#----------------------------------------------------------------------------------------------------------------------#
PROJECT(LibMeshRefinement)
ADD_LIBRARY(${PROJECT_NAME} OBJECT ${SOURCES_MAIN_MESH_REFINEMENT})
#----------------------------------------------------------------------------------------------------------------------#
# LINKED LIBRARIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_GLOBALS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_CONFIGFILES_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_DATA_STRUCTURES})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_NUMERICS_TOOLS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_LIB_MESH})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_LIB_GEOMETRY})
#----------------------------------------------------------------------------------------------------------------------#
# INCLUDE DIRECTORIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_GLOBALS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_CONFIGFILES_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_DATA_STRUCTURES})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_NUMERICS_TOOLS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_LIB_MESH})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_LIB_GEOMETRY})
#----------------------------------------------------------------------------------------------------------------------#
# GLOBAL VARIABLES
#----------------------------------------------------------------------------------------------------------------------#
SET(LINKEDLIBS_MAIN_MESH_REFINEMENT ${PROJECT_NAME} CACHE INTERNAL "")
SET(INCLUDE_DIRECTORIES_MAIN_MESH_REFINEMENT ${PROJECT_BINARY_DIR} CACHE INTERNAL "")
#----------------------------------------------------------------------------------------------------------------------#
# TARGET PROPERTIES
#----------------------------------------------------------------------------------------------------------------------#
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES COMPILE_FLAGS ${MAIN_FLAGS_OVERALL})
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIRECTORIES_LIB_MESH})
#----------------------------------------------------------------------------------------------------------------------#
