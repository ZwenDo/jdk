/*
 * Copyright (c) 2014, 2024, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
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
 * @test
 * @bug 8032908 8081392 8343001
 * @summary Test if Node.getTextContent() function correctly returns children
 * content and also check that Node.getNodeValue() returns null value for
 * Element nodes
 * @compile TestFunc.java XSLT.java
 * @run main/othervm -Djdk.xml.enableExtensionFunctions=true XSLT
 */
import java.io.ByteArrayOutputStream;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

public class XSLT {

    static final String XMLTOTRANSFORM = "/in.xml";
    static final String XSLTRANSFORMER = "/test.xsl";
    static final String EXPECTEDRESULT = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>ABCDEFG:null";

    public static void main(String[] args) throws TransformerException {
        ByteArrayOutputStream resStream = new ByteArrayOutputStream();
        TransformerFactory trf = TransformerFactory.newInstance();
        Transformer tr = trf.newTransformer(new StreamSource(System.getProperty("test.src", ".") + XSLTRANSFORMER));
        tr.transform(new StreamSource(System.getProperty("test.src", ".") + XMLTOTRANSFORM), new StreamResult(resStream));
        System.out.println("Transformation completed. Result:" + resStream.toString());
        if (!resStream.toString().equals(EXPECTEDRESULT)) {
            throw new RuntimeException("Incorrect transformation result");
        }
    }
}
