# wile
A small simple scheme compiler

Nothing to see here, move along!

################################

I have two major goals for this project:
* learn how to construct a compiler
* get a practical tool for writing programs

First, although I'm an experienced software engineer, I'm not a computer scientist by training - my background is applied math and numerical analysis. So I never studied the classical computer science curriculum, in particular compiler construction. This project is my self-study learning about compilers course.

Second, I like scheme and want to be able to write programs in scheme that I can then use or distribute. I think in the unix way of small simple tools, so that's the style of programs I want to be able to write; I don't want a fancy fully-integrated environment, either for myself or to distribute tools.

The name wile is of course the name of that extremely stable super-genius schemer, Wile E. Coyote. 'nuff said

A few other points:
* I'm going to implement this at least initially as a scheme->c compiler, not targeted to any specific machine hardware. I'm currently on x86, but may soonish switch to Apple hardware... I'd rather not deal with two different assembly languages at the moment. That should make wile more portable, but on the other hand it makes stuff like tail call optimization a bit more awkward.

* License: wile will be released under GPLv3. Its runtime library will be released under LGPL; linking your code with the runtime library will not cause your code to become GPL'd. You retain copyright of your code and may keep your code as open or closed as you desire.

* Garbage collection: I am going to initially use the Boehm garbage collector. If you don't have that installed, I intend to have compiled programs be able to run without garbage collection (but they may suck down googobs of memory). For small short programs that may be adequate.

* Number tower: I don't intend to have full bignums initially, but I do intend to have integers, rationals, and floation-point numbers. gcc and clang both support a 128-bit integer type I believe, and I plan to support 128-bit floats using gcc's libquadmath. All of that will be configurable I hope & intend.
