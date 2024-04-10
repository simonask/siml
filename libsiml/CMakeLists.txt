cmake_minimum_required(VERSION 3.0)

project(libsiml)
enable_language(C)
include(GNUInstallDirs)

find_program(CARGO
    NAMES cargo
    DOC "Path to cargo"
    REQUIRED
)
message("Found cargo: ${CARGO}")

set(CARGO_TARGET_DIR "${CMAKE_CURRENT_BINARY_DIR}/target")

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
    set(CARGO_PROFILE "debug")
elseif ("${CMAKE_BUILD_TYPE}" STREQUAL "RelWithDebInfo")
    set(CARGO_PROFILE "relwithdebinfo")
else()
    set(CARGO_PROFILE "release")
endif()

set(LIBSIML_STATICLIB "${CARGO_TARGET_DIR}/${CARGO_PROFILE}/${CMAKE_STATIC_LIBRARY_PREFIX}libsiml${CMAKE_STATIC_LIBRARY_SUFFIX}")

add_custom_command(
    OUTPUT "${LIBSIML_STATICLIB}"
    PRE_LINK
    COMMAND
        ${CARGO} build --target-dir "${CARGO_TARGET_DIR}" --profile "${CARGO_PROFILE}" -p libsiml
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/Cargo.toml"
        "${CMAKE_CURRENT_SOURCE_DIR}/src/lib.rs"
    COMMENT "Building libsiml"
)
add_custom_target(libsiml_target DEPENDS "${LIBSIML_STATICLIB}")
set_target_properties(libsiml_target PROPERTIES
    LINKER_LANGUAGE "C"
)

add_library(libsiml_static STATIC IMPORTED GLOBAL)
add_dependencies(libsiml_static libsiml_target)
set_target_properties(libsiml_static PROPERTIES
    IMPORTED_LOCATION "${LIBSIML_STATICLIB}"
    LINKER_LANGUAGE "C"
    IMPORTED_LINK_INTERFACE_LANGUAGES "C"
)
target_include_directories(libsiml_static
    INTERFACE
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>/include"
        "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)
if (WIN32)
    target_link_libraries(libsiml_static INTERFACE userenv bcrypt kernel32 user32 ws2_32 ntdll)
endif()

add_library(libsiml_shared SHARED dummy.c)
set_target_properties(libsiml_shared PROPERTIES OUTPUT_NAME siml)
target_link_libraries(libsiml_shared libsiml_static)
target_include_directories(libsiml_shared
    PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>/include"
        "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)

export(
    TARGETS libsiml_shared # TODO: Export the static library as well...
    FILE siml.cmake
)

set(CPACK_PACKAGE_NAME "libsiml")
set(CPACK_PACKAGE_VERSION "0.1.0")
install(
    TARGETS libsiml_shared
    EXPORT libsiml
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
)
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/include/siml.h" DESTINATION include)
install(FILES "${LIBSIML_STATICLIB}" DESTINATION lib)

include(CPack)


include(CTest)
enable_testing()
add_executable(siml-test test.c)
target_link_libraries(siml-test libsiml_static)
if(MSVC)
  target_compile_options(siml-test PRIVATE /W4 /c)
else()
  target_compile_options(siml-test PRIVATE -Wall -Wextra -Wpedantic)
endif()
add_test(NAME siml-test COMMAND siml-test)
