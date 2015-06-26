# R in Action

Source code for [R in Action 2nd Edition](http://www.manning.com/kabacoff2/?a_aid=RiA2ed&a_bid=5c2b1e1d)
2015 - Copyright: Manning Publications. All rights reserved.

This directory contains the source code for chapters 1 through 22 and the bonus chapter.

Although I've tried to limit the errors appearing in the book, some have crept in and are listed
in the [Errata](./R_in_Action_2nd_Edition-Errata.pdf).

Many chapters require the installation of contributed packages.
These are indicated in the header of the corresponding code file.
For example, the code in chapter 3 requires that you have the
Hmisc package installed. If you don't have this package installed, 
execute the command install.packages("Hmisc") while connected to the internet.
    
You will also find that packages may be loaded more than once in
a file. For example, in the chapter 6 code file, the statement
    "library(vcd)" 
is repeated several times. It is actually only needed once. 
However, placing it in several places allows you run a portion
of the file's code without having to run all the code from the beginning. 
    
I hope you find this code helpful and welcome your feedback.

-Rob Kabacoff, Ph.D.
robk@statmethods.net
