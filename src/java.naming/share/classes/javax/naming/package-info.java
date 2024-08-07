/*
 * Copyright (c) 1999, 2024, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

/**
 * Provides the classes and interfaces for accessing naming services.
 *
 * <p>
 * This package defines the naming operations of the Java Naming and
 * Directory Interface (JNDI). &nbsp;
 * JNDI provides naming and directory functionality to applications
 * written in the Java programming language. It is designed to be
 * independent of any specific naming or directory service
 * implementation. Thus a variety of services--new, emerging, and
 * already deployed ones--can be accessed in a common way.
 *
 *
 * <h2>Context</h2>
 * <p>
 * This package defines the notion of a <em>context</em>, represented
 * by the {@code Context} interface.
 * A context consists of a set of name-to-object <em>bindings</em>.
 * {@code Context} is the core interface for looking up, binding, unbinding,
 * and renaming objects, and for creating and destroying subcontexts.
 * <p>
 * {@code lookup()} is the most commonly used operation.
 * You supply {@code lookup()}
 * the name of the object you want
 * to look up, and it returns the object bound to that name.
 * For example, the following code fragment looks up
 * a printer and sends a document to the printer object
 * to be printed:
 *
 * {@snippet :
 * Printer printer = (Printer)ctx.lookup("treekiller");
 * printer.print(report);
 * }
 *
 * <h2>Names</h2>
 * <p>
 * Every naming method in the {@code Context}
 * interface has two
 * overloads: one that accepts a
 * {@code Name} argument and one that accepts a string name.
 * {@code Name} is an interface that represents a generic
 * name--an ordered sequence of zero of more components.
 * For these methods, {@code Name} can be used to represent a
 * <em>composite name</em> ({@code CompositeName})
 * so that you can name an object using a name which spans multiple namespaces.
 * <p>
 * The overloads that accept {@code Name}
 * are useful for applications that need to manipulate names: composing
 * them, comparing components, and so on.
 * The overloads that accept string names are likely to be more useful
 * for simple applications, such as those that simply read in a name
 * and look up the corresponding object.
 *
 * <h2>Bindings</h2>
 *
 * The {@code Binding} class represents a name-to-object binding.
 * It is a tuple containing the name of the bound object,
 * the name of the object's class, and the object itself.
 * <p>
 * The {@code Binding} class is actually a subclass of
 * {@code NameClassPair}, which consists
 * simply of the object's name and the object's class name.
 * The {@code NameClassPair} is useful when you only want
 * information about the object's class and do not want to
 * pay the extra cost of getting the object.
 *
 * <h2>References</h2>
 * Objects are stored in naming and directory services in different ways.
 * If an object store supports storing Java objects,
 * it might support storing an object in its serialized form.
 * However, some naming and directory services do not support the
 * storing of Java objects. Furthermore, for some
 * objects in the directory, Java programs are but one group of applications
 * that access them. In this case, a serialized Java object might
 * not be the most appropriate representation.
 * JNDI defines a <em>reference</em>, represented by the {@code Reference}
 * class, which contains information on how to construct a copy of the object.
 * JNDI will attempt to turn references looked up from the directory
 * into the Java objects they represent, so that
 * JNDI clients have the illusion that what
 * is stored in the directory are Java objects.
 *
 *
 * <h2>The Initial Context</h2>
 *
 * In JNDI, all naming and directory operations are performed relative
 * to a context. There are no absolute roots.
 * Therefore JNDI defines an <em>initial context</em>,
 * {@code InitialContext},
 * which provides a starting point for naming and directory operations.
 * Once you have an initial context, you can use it to
 * look up other contexts and objects.
 *
 * <h2>Exceptions</h2>
 *
 * JNDI defines a class hierarchy for exceptions that can be thrown in
 * the course of performing naming and directory operations. The root of
 * this class hierarchy is {@code NamingException}.
 * Programs interested in dealing with a particular exception
 * can catch the corresponding subclass of the exception.
 * Otherwise, programs should catch {@code NamingException}.
 *
 *
 * <h2>Package Specification</h2>
 *
 * The JNDI API Specification and related documents can be found in the
 * {@extLink jndi_overview JNDI documentation}.
 *
 * @since 1.3
 */
package javax.naming;
