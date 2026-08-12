#----------------------------------------------------------------------------------------------------------------------#
# LIB-DATA-EXPORT source files
#----------------------------------------------------------------------------------------------------------------------#
FILE(GLOB SOURCES_LIB_DATA_EXPORT
  "src/*.f90"
)
#----------------------------------------------------------------------------------------------------------------------#
#
#----------------------------------------------------------------------------------------------------------------------#
# LIB-DATA-EXPORT LIBRARY
#----------------------------------------------------------------------------------------------------------------------#
PROJECT(LibDataExport)
ADD_LIBRARY(${PROJECT_NAME} OBJECT ${SOURCES_LIB_DATA_EXPORT})
#----------------------------------------------------------------------------------------------------------------------#
# LINKED LIBRARIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_GLOBALS})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_DATA_STRUCTURES})
TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LINKEDLIBS_HDF5_TOOLS})
#----------------------------------------------------------------------------------------------------------------------#
# INCLUDE DIRECTORIES
#----------------------------------------------------------------------------------------------------------------------#
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_GLOBALS})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_DATA_STRUCTURES})
TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRECTORIES_HDF5_TOOLS})
#----------------------------------------------------------------------------------------------------------------------#
# GLOBAL VARIABLES
#----------------------------------------------------------------------------------------------------------------------#
SET(LINKEDLIBS_LIB_DATA_EXPORT ${PROJECT_NAME} CACHE INTERNAL "")
SET(INCLUDE_DIRECTORIES_LIB_DATA_EXPORT ${PROJECT_BINARY_DIR} CACHE INTERNAL "")
#----------------------------------------------------------------------------------------------------------------------#
# TARGET PROPERTIES
#----------------------------------------------------------------------------------------------------------------------#
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES COMPILE_FLAGS ${MAIN_FLAGS_OVERALL})
SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_DIRECTORIES_LIB_DATA_EXPORT})
#----------------------------------------------------------------------------------------------------------------------#
