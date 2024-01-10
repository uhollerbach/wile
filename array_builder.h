// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

/* An analog of STL::vector: an array builder. It's pure C, so we have to
   use macros to make it type-specific.

   To use this, #define the type for which arrays are to be built, and the
   type by which to refer to the builder itself:

	#define AB_ATYPE	double
	#define AB_STYPE	ab_double

   then #include this file. AB_ATYPE and AB_STYPE must not contain spaces,
   so "struct foo" would not work. Use a typedef to avoid that problem.

   Optionally, #define the prefix to use:

	#define AB_PREFIX	my_ab

   which will determine the names of the types and routines: they will be
   generated by concatenating AB_PREFIX and AB_ATYPE. The default is "AB".

   Optionally, #define AB_ERROR as the name of an error handler, a routine
   which will be called with a single char* argument.

	#define AB_ERROR	notify

   If the error handler returns, exit(1) will get called.

   The routines that are generated are:

	AB_STYPE	<pre>_setup(size_t initial)
	void		<pre>_destroy(AB_STYPE* ab)
	void		<pre>_detach(AB_STYPE* ab)
	void		<pre>_grow(AB_STYPE* ab, size_t add)
	void		<pre>_append(AB_STYPE* ab, AB_ATYPE item)
	AB_ATYPE*	<pre>_get_array(AB_STYPE* ab)
	size_t		<pre>_get_size(AB_STYPE* ab)
	void		<pre>_set_size(AB_STYPE* ab, size_t s)

   where <pre> = <AB_PREFIX>_<AB_ATYPE>_
*/

#include <stdlib.h>
#include <stdio.h>

#if (!(defined(AB_ATYPE) && defined(AB_STYPE)))
#error "Must #define AB_ATYPE and AB_STYPE before #including array_builder.h!"
#endif /* defined(AB_ATYPE) && defined(AB_STYPE) */

#ifndef AB_PREFIX
#define AB_PREFIX		ab
#endif /* AB_PREFIX */

#ifndef AB_ERROR
#define AB_ERROR(msg)		fprintf(stderr, "%s\n", msg)
#endif /* AB_ERROR */

#ifdef AB_STATIC
#undef AB_STATIC
#define AB_STATIC		static
#else
#define AB_STATIC
#endif /* AB_STATIC */

#define ABSTR1(name)		# name
#define ABSTR(name)		ABSTR1(name)
#define ABCAT1(prefix,name)	prefix ## _ ## name
#define ABCAT(prefix,name)	ABCAT1(prefix,name)

#define ABP			ABCAT(AB_PREFIX,AB_ATYPE)
#define ABR(name)		ABCAT(ABP,name)

#ifndef __GNUC__
/* Make sure __attribute__ works on non-gcc systems. Yes, might be a bit ugly */
#define __attribute__(x)
#endif

/* Not all of these functions will necessarily get used... hush warnings */
#ifdef __GNUC__
#define AB_HUSH			__attribute__((unused))
#endif /* __GNUC__ */

#ifndef AB_HUSH
#define AB_HUSH
#endif

#ifndef AB_ALLOC
#define AB_ALLOC(t,c)		((t*) malloc((c)*sizeof(t)))
#endif

#ifndef AB_REALLOC
#define AB_REALLOC(t,o,c)	((t*) realloc((o), (c)*sizeof(t)))
#endif

#ifndef AB_FREE
#define AB_FREE(p)		free(p)
#endif

/* The basic structure for managing buildable arrays. This could be made
   type-independent, by making the "array" member be a "void*"; but doing
   it this way maybe adds a tiny bit of type safety. */

typedef struct ABP {
    AB_ATYPE* array;	/* array being managed */
    size_t size;	/* current number of entries in array */
    size_t capacity;	/* capacity of the array */
} AB_STYPE;

/* Initial set up of a new array builder: the constructor. Return an AB_STYPE
   structure by value, they are pretty small. */

AB_HUSH AB_STATIC AB_STYPE ABR(setup)(size_t initial)
{
    AB_STYPE ab;

    ab.capacity = (initial > 0) ? initial : 4;
    ab.size = 0;
    ab.array = AB_ALLOC(AB_ATYPE, ab.capacity);
    if (ab.array == NULL) {
	AB_ERROR("array builder error: unable to allocate memory");
	exit(1);
    }
    return ab;
}

AB_HUSH AB_STATIC void ABR(destroy)(AB_STYPE* ab)
{
    if (ab) {
	ab->capacity = 0;
	ab->size = 0;
	AB_FREE(ab->array);
	ab->array = NULL;
    } else {
	AB_ERROR("array builder error: NULL passed into destroy");
	exit(1);
    }
}

AB_HUSH AB_STATIC void ABR(detach)(AB_STYPE* ab)
{
    if (ab) {
	ab->capacity = 0;
	ab->size = 0;
	ab->array = NULL;
    } else {
	AB_ERROR("array builder error: NULL passed into destroy");
	exit(1);
    }
}

AB_HUSH AB_STATIC void ABR(grow)(AB_STYPE* ab, size_t add)
{
    size_t ns;

    if (ab) {
	ns = ab->size + add;
	if (ab->capacity < ns) {
	    if (ab->capacity == 0) {
		ab->capacity = 1;
	    }
	    while (ab->capacity < ns) {
		ab->capacity *= 2;
	    }
	    ab->array = AB_REALLOC(AB_ATYPE, ab->array, ab->capacity);
	    if (ab->array == NULL) {
		AB_ERROR("array builder error: unable to grow array");
		exit(1);
	    }
	}
    } else {
	AB_ERROR("array builder error: NULL passed into grow");
	exit(1);
    }
}

AB_HUSH AB_STATIC void ABR(append)(AB_STYPE* ab, AB_ATYPE item)
{
    if (ab) {
	if (ab->size == ab->capacity) {
	    ABR(grow)(ab, 1);
	}
	ab->array[ab->size] = item;
	++ab->size;
    } else {
	AB_ERROR("array builder error: NULL passed into append");
	exit(1);
    }
}

AB_HUSH AB_STATIC AB_ATYPE* ABR(get_array)(AB_STYPE* ab)
{
    if (ab) {
	return ab->array;
    } else {
	AB_ERROR("array builder error: NULL passed into get_array");
	exit(1);
    }
}

AB_HUSH AB_STATIC size_t ABR(get_size)(AB_STYPE* ab)
{
    if (ab) {
	return ab->size;
    } else {
	AB_ERROR("array builder error: NULL passed into get_size");
	exit(1);
    }
}

AB_HUSH AB_STATIC void ABR(set_size)(AB_STYPE* ab, size_t s)
{
    if (ab) {
	if (s < ab->size) {
	    ab->size = s;
	}
    } else {
	AB_ERROR("array builder error: NULL passed into set_size");
	exit(1);
    }
}

/* Clean up so we can #include this again */

#undef AB_ATYPE
#undef AB_STYPE
#undef AB_PREFIX
#undef AB_ERROR
#undef AB_STATIC
#undef AB_HUSH
#undef ABP
#undef ABR

#undef ABSTR1
#undef ABSTR
#undef ABCAT1
#undef ABCAT
