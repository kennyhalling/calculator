# Overview

This is an Erlang program designed to do basic arithmetic like addition and subtraction, as well some more complex operations like log10 and natural log. I also programed this calculator to keep track of past calculations in a "history" list. All of these operations and functionalities are stored in a single process loop that recursively calls inself with updated history each time an operation is called.

I wrote this program to expand my abilities with Erlang, a languange I have very little experience in. I specifically wanted to get better at recursion and in understanding what a process in Erlang is.

[Software Demo Video](https://www.youtube.com/watch?v=aB5AbQlCHsI&ab_channel=KennethHalling)

# Development Environment

To develop this program I used VS Code to write the code, as well as WSL on Windows to actually call functions from the program I was writing.

I wrote this program in the Erlang language, using the rebar3 build tool to compile and call functions in the terminal.

# Useful Websites

- [Erlang Wikipedia](https://en.wikipedia.org/wiki/Erlang_(programming_language))
- [Erlang Documentation](https://www.erlang.org/faq/introduction.html)

# Future Work

{Make a list of things that you need to fix, improve, and add in the future.}
* In the future, I want to build a chaining feature that allows a user to call a function, take that result, and immediately put it into a new function.
* I want to make interacting with the program a little easier, potentially through a GUI, or just by simplfying what a user needs to enter into the terminal.
* I want to add some premade equations that a user can call such as Pythagorean's Theorem and the Quadractic Formula.
