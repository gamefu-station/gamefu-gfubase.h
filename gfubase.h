/// ======================================================================= ///
/// This file is part of the GameFU System project                          ///
/// ----------------------------------------------------------------------- ///
/// Copyright (C) 2025  Local Atticus <contact@nashiora.com>                ///
///                                                                         ///
/// This software is provided 'as-is', without any express or implied       ///
/// warranty. In no event will the authors be held liable for any damages   ///
/// arising from the use of this software.                                  ///
///                                                                         ///
/// Permission is granted to anyone to use this software for any purpose,   ///
/// including commercial applications, and to alter it and redistribute it  ///
/// freely, subject to the following restrictions:                          ///
///                                                                         ///
/// 1. The origin of this software must not be misrepresented; you must not ///
///    claim that you wrote the original software. If you use this software ///
///    in a product, an acknowledgment in the product documentation would   ///
///    be appreciated but is not required.                                  ///
///                                                                         ///
/// 2. Altered source versions must be plainly marked as such, and must not ///
///    be misrepresented as being the original software.                    ///
///                                                                         ///
/// 3. This notice may not be removed or altered from any source            ///
///    distribution.                                                        ///
/// ======================================================================= ///

#ifndef GFUBASE_H_
#define GFUBASE_H_

/// ======================================================================= ///
/// Standard headers, for convenience                                       ///
/// ======================================================================= ///

#include <assert.h>
#include <inttypes.h>
#if __STDC_VERSION__ < 202311L
#    include <stdalign.h>
#endif
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// ======================================================================= ///
/// Platform and compiler identification                                    ///
/// ======================================================================= ///

#define GFU_WINDOWS 0
#define GFU_LINUX   0

#define GFU_GCC     0
#define GFU_CLANG   0
#define GFU_MSVC    0

#ifdef _WIN32
#    undef GFU_WINDOWS
#    define GFU_WINDOWS 1
#endif

#ifdef __linux__
#    undef GFU_LINUX
#    define GFU_LINUX 1
#endif

#ifdef __GNUC__
#    undef GFU_GCC
#    define GFU_GCC 1
#endif

#ifdef __clang__
#    undef GFU_CLANG
#    define GFU_CLANG 1
#endif

#if defined(_MSCVER) && !GFU_CLANG
#    undef GFU_MSVC
#    define GFU_MSVC 1
#endif

/// ======================================================================= ///
/// "Fancy" macros                                                          ///
/// ======================================================================= ///

#if __STDC_VERSION__ < 202311L || (defined(_MSC_VER) && !defined(__clang__))
#    define nullptr NULL
#endif

#define gfu_cast(T) (T)
#define gfu_discard (void)

#define gfu_return_defer(Value) do { result = (Value); goto defer; } while (0)
#define gfu_scope_defer(Expr) for (int gfu_scope_defer_i_##__LINE__ = 0; gfu_scope_defer_i_##__LINE__ == 0; ((gfu_scope_defer_i_##__LINE__ = 1), (Expr))) do { } while (0)

#define gfu_assert(Cond) assert(Cond)
#define gfu_assert_message(Cond, Message) assert(Cond && "" Message "")

/// ======================================================================= ///
/// Primitive types                                                         ///
/// ======================================================================= ///

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef ptrdiff_t isize;
typedef size_t usize;

typedef float f32;
typedef double f64;

/// ======================================================================= ///
/// Allocators                                                              ///
/// ======================================================================= ///

typedef enum gfu_allocator_action {
    GFU_ALLOC,
    GFU_REALLOC,
    GFU_REALLOC2,
    GFU_DEALLOC,
} gfu_allocator_action;

typedef void* (*gfu_allocator_function)(void* userdata, gfu_allocator_action alloc_action, void* memory, isize size, isize previous_size, isize align);

typedef struct gfu_allocator {
    gfu_allocator_function allocator_function;
    void* userdata;
} gfu_allocator;

typedef struct gfu_arena_block {
    char* data;
    isize consumed;
} gfu_arena_block;

typedef struct gfu_arena {
    gfu_allocator* allocator;
    gfu_arena_block* data;
    isize capacity, count;
    isize block_size;
} gfu_arena;

extern gfu_allocator gfu_allocator_default;
extern gfu_allocator gfu_allocator_temp;

i64 gfu_align_for_bytes(isize byte_count);
isize gfu_align_padding(isize size, isize align);
isize gfu_align_to(isize size, isize align);

void* gfu_alloc(gfu_allocator* allocator, isize size);
void* gfu_alloc_aligned(gfu_allocator* allocator, isize size, isize align);
void* gfu_realloc(gfu_allocator* allocator, void* memory, isize size);
void* gfu_realloc_aligned(gfu_allocator* allocator, void* memory, isize size, isize align);
void* gfu_realloc2(gfu_allocator* allocator, void* memory, isize previous_size, isize size);
void* gfu_realloc2_aligned(gfu_allocator* allocator, void* memory, isize previous_size, isize size, isize align);
void gfu_dealloc(gfu_allocator* allocator, void* memory);

isize gfu_temp_mark();
void gfu_temp_rewind(isize mark);
void gfu_temp_clear();

void gfu_arena_init(gfu_arena* arena, gfu_allocator* allocator, isize block_size);
void gfu_arena_deinit(gfu_arena* arena);
void* gfu_arena_alloc(gfu_arena* arena, isize size);
void* gfu_arena_alloc_aligned(gfu_arena* arena, isize size, isize align);
void* gfu_arena_realloc2(gfu_arena* arena, void* memory, isize previous_size, isize size);
void* gfu_arena_realloc2_aligned(gfu_arena* arena, void* memory, isize previous_size, isize size, isize align);
void gfu_arena_clear(gfu_arena* arena);

/// ======================================================================= ///
/// Container types                                                         ///
/// ======================================================================= ///

#define GFU_DA_INITIAL_CAPACITY 256

#define GFU_DYNAMIC_ARRAY_FIELDS(ElemType) gfu_allocator* allocator; ElemType* data; isize capacity, count

#define gfu_da_ensure_capacity(DynArr, MinCap) do {                                                     \
        gfu_dynamic_array_ensure_capacity((DynArr)->allocator, gfu_cast(isize) sizeof(*(DynArr)->data), \
            gfu_cast(void**) &(DynArr)->data, &(DynArr)->capacity, (MinCap));                           \
    } while (0)

#define gfu_da_push(DynArr, Elem) do {                         \
        gfu_da_ensure_capacity((DynArr), (DynArr)->count + 1); \
        gfu_assert((DynArr)->data != nullptr);                 \
        (DynArr)->data[(DynArr)->count++] = (Elem);            \
    } while (0)

#define gfu_da_push_many(DynArr, Elems, ElemCount) do {                                         \
        gfu_da_ensure_capacity((DynArr), (DynArr)->count + (ElemCount));                        \
        memcpy((DynArr)->data + (DynArr)->count, Elems, sizeof(*(DynArr)->data) * (ElemCount)); \
        (DynArr)->count += (ElemCount);                                                         \
    } while (0)

#define gfu_da_pop(DynArr) (gfu_assert((DynArr)->data != nullptr), (DynArr)->data[--(DynArr)->count])

#define gfu_da_dealloc(DynArr) do { gfu_dealloc((DynArr)->allocator, (DynArr)->data); } while (0)

void gfu_dynamic_array_ensure_capacity(gfu_allocator* allocator, isize element_size, void** da_data_ptr, i64* da_capacity_ptr, i64 required_capacity);

/// ======================================================================= ///
/// String types                                                            ///
/// ======================================================================= ///

#define GFU_STR_FMT "%.*s"
#define GFU_STR_ARG(Str) (int)(Str).count, (Str).data

#define GFU_SV_CONST(ConstStr) (gfu_string_view){ .data = "" ConstStr "", .count = gfu_cast(isize) ((sizeof(ConstStr) / sizeof(char)) - 1) }

typedef struct gfu_string {
    GFU_DYNAMIC_ARRAY_FIELDS(char);
} gfu_string;

typedef struct gfu_string_view {
    const char* data;
    isize count;
} gfu_string_view;

isize gfu_cstrlen(const char* cstr);
isize gfu_cstrnlen(const char* cstr, isize max_length);

isize gfu_string_append_cstr(gfu_string* str, const char* cstr);
isize gfu_string_append_sv(gfu_string* str, gfu_string_view sv);
isize gfu_string_sprintf(gfu_string* str, const char* format, ...);
isize gfu_string_vsprintf(gfu_string* str, const char* format, va_list v);

gfu_string_view gfu_sv(const char* cstr, isize length);
gfu_string_view gfu_string_as_view(gfu_string* str);
gfu_string_view gfu_cstr_as_view(const char* cstr);
gfu_string_view gfu_sv_slice(gfu_string_view sv, isize offset, isize length);

#endif /* GFUBASE_H_ */


#ifdef GFUBASE_IMPLEMENTATION

/// ======================================================================= ///
/// Allocators                                                              ///
/// ======================================================================= ///

static void* gfu_allocator_default_function(void* userdata, gfu_allocator_action alloc_action, void* memory, isize size, isize previous_size, isize align);

gfu_allocator gfu_allocator_default = {
    .allocator_function = gfu_allocator_default_function,
};

static void* gfu_allocator_temp_function(void* userdata, gfu_allocator_action alloc_action, void* memory, isize size, isize previous_size, isize align);

static char gfu_allocator_temp_buffer[64 * 1024 * 1024];
static isize gfu_allocator_temp_allocated = 0;

gfu_allocator gfu_allocator_temp = {
    .allocator_function = gfu_allocator_temp_function,
};

isize gfu_align_for_bytes(isize byte_count) {
    byte_count &= 0x7FFF;

    /// https://graphics.stanford.edu/%7Eseander/bithacks.html#RoundUpPowerOf2
    ///
    /// Devised by
    ///   - Sean Anderson, Sepember 14, 2001
    ///   - Peter Hart and William Lewis, February 14, 1997
    ///
    /// Computes the next power of two >= 'bytes'.
    isize align = byte_count - 1;
    align |= align >> 1;
    align |= align >> 2;
    align |= align >> 4;
    align |= align >> 8;
    return align + 1;
}

isize gfu_align_padding(isize size, isize align) {
    return (align - (size % align)) % align;
}

isize gfu_align_to(isize size, isize align) {
    return size + gfu_align_padding(size, align);
}

void* gfu_alloc(gfu_allocator* allocator, isize size) {
    if (allocator == nullptr) {
        allocator = &gfu_allocator_default;
    }

    return allocator->allocator_function(allocator->userdata, GFU_ALLOC, nullptr, 0, size, 16);
}

void* gfu_alloc_aligned(gfu_allocator* allocator, isize size, isize align) {
    if (allocator == nullptr) {
        allocator = &gfu_allocator_default;
    }

    return allocator->allocator_function(allocator->userdata, GFU_ALLOC, nullptr, 0, size, align);
}

void* gfu_realloc(gfu_allocator* allocator, void* memory, isize size) {
    if (allocator == nullptr) {
        allocator = &gfu_allocator_default;
    }

    return allocator->allocator_function(allocator->userdata, GFU_REALLOC, memory, 0, size, 16);
}

void* gfu_realloc_aligned(gfu_allocator* allocator, void* memory, isize size, isize align) {
    if (allocator == nullptr) {
        allocator = &gfu_allocator_default;
    }

    return allocator->allocator_function(allocator->userdata, GFU_REALLOC, memory, 0, size, align);
}

void* gfu_realloc2(gfu_allocator* allocator, void* memory, isize previous_size, isize size) {
    if (allocator == nullptr) {
        allocator = &gfu_allocator_default;
    }

    return allocator->allocator_function(allocator->userdata, GFU_REALLOC2, memory, previous_size, size, 16);
}

void* gfu_realloc2_aligned(gfu_allocator* allocator, void* memory, isize previous_size, isize size, isize align) {
    if (allocator == nullptr) {
        allocator = &gfu_allocator_default;
    }

    return allocator->allocator_function(allocator->userdata, GFU_REALLOC2, memory, previous_size, size, align);
}

void gfu_dealloc(gfu_allocator* allocator, void* memory) {
    if (allocator == nullptr) {
        allocator = &gfu_allocator_default;
    }

    gfu_discard allocator->allocator_function(allocator->userdata, GFU_DEALLOC, memory, 0, 0, 0);
}


isize gfu_temp_mark() {
    return gfu_allocator_temp_allocated;
}

void gfu_temp_rewind(isize mark) {
    gfu_allocator_temp_allocated = mark;
}

void gfu_temp_clear() {
    memset(gfu_allocator_temp_buffer, 0, gfu_cast(usize) gfu_allocator_temp_allocated);
    gfu_allocator_temp_allocated = 0;
}


void gfu_arena_init(gfu_arena* arena, gfu_allocator* allocator, isize block_size) {
    if (arena == nullptr) return;
    *arena = (gfu_arena){
        .allocator = allocator,
        .block_size = block_size,
    };
}

void gfu_arena_deinit(gfu_arena* arena) {
    if (arena == nullptr) return;

    for (isize i = 0; i < arena->count; i++) {
        gfu_dealloc(arena->allocator, arena->data[i].data);
    }

    gfu_da_dealloc(arena);
    *arena = (gfu_arena) {0};
}

void* gfu_arena_alloc(gfu_arena* arena, isize size) {
    return gfu_arena_realloc2(arena, nullptr, 0, size);
}

void* gfu_arena_alloc_aligned(gfu_arena* arena, isize size, isize align) {
    return gfu_arena_realloc2_aligned(arena, nullptr, 0, size, align);
}

void* gfu_arena_realloc2(gfu_arena* arena, void* memory, isize previous_size, isize size) {
    return gfu_arena_realloc2_aligned(arena, memory, previous_size, size, 16);
}

void* gfu_arena_realloc2_aligned(gfu_arena* arena, void* memory, isize previous_size, isize size, isize align) {
    if (memory != nullptr && previous_size >= size) {
        return memory;
    }

    if (size == 0) {
        return nullptr;
    }

    gfu_assert(arena != nullptr);
    gfu_assert(arena->block_size > 0);
    gfu_assert(align > 0);

    gfu_arena_block* block = nullptr;

    for (isize i = 0; i < arena->count; i++) {
        gfu_arena_block* check_block = &arena->data[i];
        if (arena->block_size - gfu_align_to(check_block->consumed, align) >= size) {
            block = check_block;
            break;
        }
    }

    if (block == nullptr) {
        gfu_da_push(arena, ((gfu_arena_block){ .data = gfu_alloc(arena->allocator, arena->block_size) }));
        block = &arena->data[arena->count - 1];
    }

    gfu_assert(block != nullptr);

    block->consumed = gfu_align_to(block->consumed, align);
    char* new_memory = block->data + block->consumed;
    block->consumed += size;

    isize zero_offset = 0;
    isize zero_length = size;

    if (memory != nullptr && previous_size > 0) {
        memcpy(new_memory, memory, gfu_cast(usize) previous_size);
        memset(memory, 0, gfu_cast(usize) previous_size);
        zero_offset += previous_size;
        zero_length -= previous_size;
    }

    memset(new_memory + zero_offset, 0, gfu_cast(usize) zero_length);
    return new_memory;
}

void gfu_arena_clear(gfu_arena* arena) {
    if (arena == nullptr) return;

    for (isize i = 0; i < arena->count; i++) {
        arena->data[i].consumed = 0;
    }
}

static void* gfu_allocator_default_function(void* userdata, gfu_allocator_action alloc_action, void* memory, isize size, isize previous_size, isize align) {
    // TODO(local): Aligned allocation in the "default" libc allocator, if possible.
    switch (alloc_action) {
        case GFU_ALLOC: return malloc(gfu_cast(usize) size);
        case GFU_REALLOC:
        case GFU_REALLOC2: return realloc(memory, gfu_cast(usize) size);
        case GFU_DEALLOC: free(memory); return nullptr;
    }
}

static void* gfu_allocator_temp_function(void* userdata, gfu_allocator_action alloc_action, void* memory, isize size, isize previous_size, isize align) {
    gfu_allocator_temp_allocated = gfu_align_to(gfu_allocator_temp_allocated, align);
    switch (alloc_action) {
        default: gfu_assert_message(false, "Unsupported allocator action on temp memory."); return nullptr;
        case GFU_ALLOC: {
            if (size == 0) return nullptr;

            char* new_memory = gfu_allocator_temp_buffer + gfu_allocator_temp_allocated;
            gfu_allocator_temp_allocated += size;

            memset(new_memory, 0, gfu_cast(usize) size);
            return new_memory;
        }

        case GFU_REALLOC2: {
            if (memory != nullptr && previous_size >= size) return memory;
            if (size == 0) return nullptr;

            char* new_memory = gfu_allocator_temp_buffer + gfu_allocator_temp_allocated;
            gfu_allocator_temp_allocated += size;

            isize zero_offset = 0;
            isize zero_length = size;

            if (memory != nullptr && previous_size > 0) {
                memcpy(new_memory, memory, gfu_cast(usize) previous_size);
                memset(memory, 0, gfu_cast(usize) previous_size);
                zero_offset += previous_size;
                zero_length -= previous_size;
            }

            memset(new_memory + zero_offset, 0, gfu_cast(usize) zero_length);
            return new_memory;
        }

        case GFU_DEALLOC: return nullptr;
    }
}

/// ======================================================================= ///
/// Container types                                                         ///
/// ======================================================================= ///

void gfu_dynamic_array_ensure_capacity(gfu_allocator* allocator, isize element_size, void** da_data_ptr, isize* da_capacity_ptr, isize required_capacity) {
    if (*da_capacity_ptr >= required_capacity) {
        return;
    }

    if (*da_data_ptr == nullptr || *da_capacity_ptr == 0) {
        *da_capacity_ptr = GFU_DA_INITIAL_CAPACITY;
        *da_data_ptr = gfu_alloc(allocator, GFU_DA_INITIAL_CAPACITY * element_size);
    } else {
        isize old_capacity = *da_capacity_ptr;

        isize new_capacity = *da_capacity_ptr;
        while (new_capacity < required_capacity) {
            new_capacity *= 2;
        }

        gfu_assert(new_capacity > 0);

        *da_capacity_ptr = new_capacity;
        *da_data_ptr = gfu_realloc2(allocator, *da_data_ptr, old_capacity * element_size, new_capacity * element_size);
    }
}

/// ======================================================================= ///
/// String types                                                            ///
/// ======================================================================= ///

isize gfu_cstrlen(const char* cstr) {
    isize length = 0;
    while (cstr[length] != '\0') {
        length++;
    }

    return length;
}

isize gfu_cstrnlen(const char* cstr, isize max_length) {
    isize length = 0;
    while (length < max_length && cstr[length] != '\0') {
        length++;
    }

    return length;
}

isize gfu_string_append_cstr(gfu_string* str, const char* cstr) {
    isize cstr_length = gfu_cstrlen(cstr);
    gfu_da_push_many(str, cstr, cstr_length);
    return cstr_length;
}

isize gfu_string_append_sv(gfu_string* str, gfu_string_view sv) {
    gfu_da_push_many(str, sv.data, sv.count);
    return sv.count;
}

isize gfu_string_sprintf(gfu_string* str, const char* format, ...) {
    va_list v;
    va_start(v, format);
    isize result_count = gfu_string_vsprintf(str, format, v);
    va_end(v);
    return result_count;
}

isize gfu_string_vsprintf(gfu_string* str, const char* format, va_list v) {
    va_list v0;
    va_copy(v0, v);
    isize check_append_count = vsnprintf(NULL, 0, format, v0);
    va_end(v0);

    gfu_da_ensure_capacity(str, str->count + check_append_count + 1);
    gfu_assert(str->data != nullptr);

    va_list v1;
    va_copy(v1, v);
    isize append_count = vsnprintf(str->data + str->count, check_append_count + 1, format, v1);
    gfu_assert(append_count == check_append_count);
    va_end(v1);

    str->count += append_count;
    return append_count;
}

gfu_string_view gfu_sv(const char* cstr, isize length) {
    return (gfu_string_view) {
        .data = cstr,
        .count = length,
    };
}

gfu_string_view gfu_string_as_view(gfu_string* str) {
    return gfu_sv(str->data, str->count);
}

gfu_string_view gfu_cstr_as_view(const char* cstr) {
    return gfu_sv(cstr, gfu_cstrlen(cstr));
}

gfu_string_view gfu_sv_slice(gfu_string_view sv, isize offset, isize length) {
    return gfu_sv(sv.data + offset, length);
}

#endif /* GFUBASE_IMPLEMENTATION */
