# SodaTime, Port of JodaTime for Scala/Scala.js

[![Join the chat at https://gitter.im/mdedetrich/soda-time](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/mdedetrich/soda-time?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/mdedetrich/soda-time.svg?branch=master)](https://travis-ci.org/mdedetrich/soda-time)

SodaTime is a port of [JodaTime](http://www.joda.org/joda-time/) to Scala, so that it can be compiled with `Scala.js`.
The intention is to have a cross compiled, high quality Date/Time library that can be used across all JVM's, as well as
`Scala.js`

## Goals
- Be completely API compatible with `JodaTime` for both Scala (JVM) and as much as possible for `Scala.js`. Please see notable changes for more info.

## Artifacts

Current `SNAPSHOT` artifacts are deployed to SonaType Snapshots, so you should be able to use the current version using

```scala
"org.mdedetrich" %% "soda-time" % "0.0.1-SNAPSHOT"
```

## What still needs to be done

This is still **ALPHA** quality, Joda Time is a big library, and there is stuff that still needs to be done. Definitely try it out,
but I wouldn't recommend using it in production.

- [ ] Take a look at sections of the converted code that used `continue`/`break` to a label.
- [ ] Tests
- [ ] v2.8.x of the Joda-Time API (v2.7.x is what is currently being implemented)
- [ ] `java.util.Locale` needs to be implemented
- [ ] Possible better solution for aux constructors
- [ ] Remove all mentions of file/io/streams from `Javascript` based API
- [ ] Adding utility methods for `Javascript` (i.e. java Date constructors)

## Why
The state of Date/Time libraries for `Java` is pretty grim (in fact its grim for most languages that haven't been made in the
past decade). Because of this, [JodaTime](http://www.joda.org/joda-time/) was created, which provided a high quality
implementation of Date/Time library for Java.

In regards to Scala, most people just use [JodaTime](http://www.joda.org/joda-time/), and the few who have completely 
migrated to `JDK-8` use `JSR-310`. Unfortunately, this poses a problem for people intending to use `Scala.js` 
(and possible any other future backend for Scala).

- `Scala.js` can only compile Scala source code, not Java source code/JVM bytecode. This means it won't
work with [JodaTime](http://www.joda.org/joda-time/)
- The `JSR-310` (designed to supersede [JodaTime](http://www.joda.org/joda-time/)) has a GPL style license, which is incompatible with `Scala.js`'s license. This
means that to implement `JSR-310` (i.e. `java.time.*`) for `Scala.js`, a complete cleanroom implementation of `java.time.*` needs to be 
done. Please see this [ticket](https://github.com/scala-js/scala-js/issues/1618) for more info.
- Even with `JSR-310` implemented, users who run on `JDK-7` and less will still be stuck. Although there are backports for `JSR-310`,
it is still unknown whether these backports will work in context of `Scala.js` (needs to be confirmed)
- Implementing a **correct** Date/Time library is really hard (see this [video](https://www.youtube.com/watch?v=-5wpm-gesOY) 
for more info)

Due to all of the above, the only real solution (at least for the short/medium term) is to have a cross compatile version of
[JodaTime](http://www.joda.org/joda-time/) that will work on `Java/Scala/Scala.js`). The ultimate solution would be to provide
a clean version of a Date/Time library code Scala (something along the lines `scala.time.*`) that would be in the Scala `stdlib`.
However due to the difficulty of coding a correct Date/Time library (as mentioned before), plus other reasons, we shouldn't be 
expecting this any time soon.

There are other reasons (outside of compatibility/cross platform) as to why you might want to use SodaTime.

- Its a clean implementation of Date/Time for javascript (unlike, for example, [moment.js](http://momentjs.com/), which uses `Javascript`'s
`Date` object behind the scenes). `Javascript` clients (including web browsers) are notorious for having quirks in how they implement the `Javascript` `Date` object 
(see [here](https://github.com/ChiperSoft/Kalendae/issues/134) as an example). `SodaTime` would not have an issue in this regard since it provides a 
correct implementation of Date/Time across all browsers (assuming that UTC timestamp retrieved from `Date.getMilliseconds` is correct)
- `JodaTime` has been battle tested for 13 years, and its the defacto standard for Date/Time on Java. This means its well tested. Any Java developer
who is worth their salt uses `JodaTime`. `SodaTime` was mainly ported using the [Scalagen](https://github.com/mysema/scalagen) library, which means that
the majority of critical business logic code has been ported correctly and automatically. Please see methodology for more info
- It has a very good design (even when used in `Scala`), due to it putting high emphasis on using immutable types

## Differences
### Breaking API differences
There are difference for `SodaTime` on Scala.js, mainly due dealing with Javascript. In regards to API, there are *some* breaking changes, which are noted below

- Methods that deal with file operations (i.e. `java.io.File`) are not exported, as they make no sense on `Javascript`
- Error classes, such as `org.joda.IllegalFieldValueException`, only have a single primary constructor, rather than various constructors as
 the original `JodaTime`. This is because of a `Scala` limitation that does not allow you to have different `super` calls within different
 constructors. Since `IllegalFieldValueException` extends a class we have no control over (`java.lang.IllegalArgumentException`) we had no
 choice but to do this. Luckily, the use case for users making `IllegalFieldValueException` with custom error messages is non existent.

### Other API changes
These are API changes which aren't breaking (i.e. usually the addition of certain utility methods)
- Constructors for `DateTime` for the `Javascript` `Date` object, i.e. from `Scala`
```scala
val dateTime = new org.joda.DateTime(new js.Date())
```

And also from `Javascript`
```javascript
var dateTime = new org.joda.DateTime(new Date());
```


## Methodology
The main goal for `SodaTime` is to provide a correct implementation of `JodaTime` for `Scala`/`Scala.js`. At the same time, `JodaTime` itself is a massive library,
so providing a clean, idiomatic Scala implementation of `JodaTime` is unrealistic and unwise. Such effort should be used in creating a new Scala implementation 
of a Date/Time library.

Hence to verify the correctness of SodaTime, we rely on the following principles

- The `JodaTime` implementation is itself is "correct". Note that JodaTime itself may not be correct, but if this is the case, then we
want `SodaTime` to simulate this
- Following on, the code that is converted from `JodaTime` using `ScalaGen` will be mainly correct in regards to
business logic (see below for more details)
- Converting the test cases from `JodaTime`, and implementing our own.

With this in mind, we generally want to leverage as many tools/methods to obtain this goal, described below

1. Convert the `Java` code to `Scala` code using `ScalaGen`, one can also use [javatoscala](http://javatoscala.com/). `ScalaGen` is not perfect, and it has issues
with the following (in order of being problematic)
    1. break/continue/break to label. Only `break` is supported in `Scala`, (and that is through an explicit import, `scala.util.control.Breaks._`). This is the
    only known change that `ScalaGen` does which actually breaks business logic (at least on a semantic level) apart from switch statements. `ScalaGen` 
    will usually output commented code such as `// break` or `// continue` in this case
    . To fix this, the following is done
        - If its only a `break`, we do use `scala.util.control.Break`
        - If its a continue, we simulate the behaviour using a flag `name `continueFlag`) in code, along with a simple `if` condition.
        - If its a break to label, we have to manually rewrite the code.
    we have to carefully rewrite the code 
    2. `ScalaGen` will try to convert `switch` statement to `match`, however it usually doesn't fully work for because of the existence/
    non existence of break. Here are the following issues with this
        - It Typically places the Scala equivalent of `default` (`case _ => `) statement at the top of the match block, which is obviously
        semantically different to how the `default` works in switch. `Scala` compiler will emit a warning to detect this, and its an
        easy fix (move the `case _ => ` to the bottom of the match statement). There are however 
        - `Java` switch uses `break`. Depending on what the switch statement does, this may or may not be semantically equivalent. As
        an example, the following code attempts to mutate a variable
            ```java
            String seconds;
            switch (getSeconds()) {
                default: 
                    seconds = "0";
                    break;
                case 1:
                    seconds = "one";
                    break;
            }
            ```
            
            The equivalent can be converted to
            
            ```scala
            val seconds = getSeconds() match {
              case 1 => "1"
              case _ => "0"
            }
            ```
            
            Sometimes its not so straight forward, especially when there is a combination of side effects/mutation. Generally speaking,
            generated match statements should be inspected
    3. Generally doesn't work with constructors/super/subclassing. This is mainly due to the fact that `Scala` doesn't support all of the `Java`s methdods of
    instantiating a new class. As an example, `Scala` has no API equivalent of supporting multiple different `super` calls in different constructors of the same class.
    When this occurs, we either use auxiallary constructors (named `auxConstructor`) if we have control of the classes that is being extended, else we resort making an
    API incompatible change which involves manually creating constructors in a companion object (see `org.joda.IllegalFieldValueException` for more info). In this sitaution,
    the following this done
        - Attempt to manually rewrite the constructors. If there is a common super constructor, we can avoid the before mentioned limitation
        - If the above isn't the case, we create an empty constructor (which doesn't initialize any state), and then we make `auxConstructors`
        in the super class to simulate the same behaviour.
        - If we don't have control over the super class (i.e the case with `org.joda.IllegalFieldValueException`), we have to make a breaking `API`
        change, and use factory instant creation methods in the companion object. So far, this has only occurred for exception classes
        - Not onlining parameters properly. `JodaTime` uses the Java conversion of using constructors to set internal private mutable variables. `ScalaGen`
        usually tries to move these online private variables into the constructor, which combined with the general super/constructor issues,
        causes problems. This code is often rewritten to resemble the `Java` equivalent
    4. Side effect statements. The `Java` code written in `JodaTime` has some parts which is written like old C style, with the
    equivalent expressions either not existing in `Scala`, or being semantically different. Examples include
        - The following `Java` code
            ```java
                if (c = getSomeChar()) {
                    // Do stuff
                };
            ```
            
            Is legitimate, and the return type of that code is `Char` (assuming that `getSomeChar()` returns a `Char` and type of `c` is `Char`).
            The equivalent (which is what `ScalaGen` outputs) returns type `Unit`, which often means the code needs to be rewritten to
            something like
            
            ```scala
                if ({c = getSomeChar();c}) {
                    // Do stuff
                }
            ```
        - Another example is double assignment, which is often used in `C`, i.e.
            ```java
            someVar = anotherVar = yetAnotherVar;
            ```
            
            This has no `Scala` equivalent, so its rewritten to
            
            ```scala
            someVar = anotherVar
            anotherVar = yetAnotherVar
            ```
        - There is also the shorthand increment operation, which often doesn't work in `Scala`. i.e.
            ```java
            Int counter = 0;
            counter += 1;
            ```
            
            In scala this is done like so
            
            ```scala
            var counter = 0
            counter = counter + 1
            ```
    5. Not creating `var`'s when variable referenced is method a parameter. As an example, `ScalaGen` usually creates the following
        ```scala
              def toDateTime(zone: DateTimeZone): DateTime = {
                zone = DateTimeUtils.getZone(zone)
                if (getZone == zone) {
                  return this
                }
                super.toDateTime(zone)
              }
        ```
        
        Which wont compile, so this is what is often done
        
        ```scala
              override def toDateTime(zone: DateTimeZone): DateTime = {
                var _zone = zone
                _zone = DateTimeUtils.getZone(_zone)
                if (getZone == _zone) {
                  return this
                }
                super.toDateTime(_zone)
              }
        ```
    6. Not adding overrides. `ScalaGen` sometimes won't create overrides for functions which override super members. This isn't really
    the fault of the tool, since `@Override` is a convention in `Java`, where as the  `Scala` compiler will force you to use `override` 
    if you are overriding the super member. Thankfully, `IntelliJ` has an inspection which picks this up automatically
    7. `ScalaGen` doesn't correctly generate for loops for `Java` code that uses the `Consumer` API. It will convert the `Java`
    for statement
        ```java
        for (item : someCollection) {
            \\ Do stuff
        };
        ```
        
        Into this
        
        ```scala
        
        for (item <- someCollection) {
            \\ Do stuff
        }
        ```
        
        The scala converted code will show as correct in some tools (i.e. Intellij), but it won't compile. Scala doesn't (yet) have
        interopt for the Java consumer API. This means the above code will be typically changed to
        
        ```scala
        val iterator = someCollection.iterator
        
        while(iterator.hasNext) {
            // Do stuff here
        }
        ```
        
    8. Manually annotate types, `Scalagen` by default doesn't annotate converted variable declarations types. This usually isn't an issue,
    but there are instances of implicitly converted types which don't work out (`i.e.` conversions between `Long`/`Int`, and vice versa).
    Manually specifying the type (i.e. `var millis:Long`) fixes this problem
2. At this point, the code will usually compile, so we do the following things
    - Really quick and easy syntax fixes. As an example, `ScalaGen` sometimes unnecessarily puts statements in extra enclosing parenthesis, amongst other things
3. Split out the code into `js` and `jvm` if it makes sense to do so. The following are reasons why you would do this
    - Replace internal collections used in business logic to ones that make sense and/or ones that have JS equivalent (performance). Examples
    includes
        - Using `js.Array` instead of `Array` internally for performance reasons
        - Changing `java.util.concurrent.ConcurrentHashMap` to standard `java.util.HashMap` internally (`Javascript` has no concept of
        multithreading, so concurrent data structures are not required)
        - In general using Scala collections over Java ones for `Javascript` implementation. This is because `Scala.js` 
        uses a deep linking optimizer, and people are much more likely to use Scala collections than Java ones, 
        saving room on theoretical outputted `Javascript` size. `JVM` implementation is untouched, as there is little point in converting
        it to `Scala` collections
    - Providing alternate types to the `opaque` types to be @JSExported. This is done just for the `Javascript` access to the API
    - Adding extra constructors for JS types (i.e. `Javascripts` array as `js.Array` as well as `scala.Array`)
    - Separate implementations of annotations that don't make sense on `Javascript`. i.e., for `joda.convert.ToString`, the `JVM` version
     is a `Java` source that looks like this (which is an exact copy of the source from `JodaTime`)
     ```java
             @Target(ElementType.METHOD)
             @Retention(RetentionPolicy.RUNTIME)
             public @interface ToString {
             
             }
     ```
     
     The JS version looks like this
     
     ```scala
         import scala.annotation.StaticAnnotation
         
         class ToString extends StaticAnnotation
     ```