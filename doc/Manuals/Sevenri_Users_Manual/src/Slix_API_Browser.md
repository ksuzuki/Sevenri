## 3.1 API-browser

slix name: `api-browser`

API-browser is a very small slix. API-browser searches an API document URL using a keyword and open the URL in the default system browser. It opens a small popup window for a very short time only when it didn't find URL in order to indicate the failure.

Currently API-browser accepts only a Java class or interface name as lookup keyword (letter case is ignored) and searches API document URL of Java SE 6 and EE 6.

You can open API-browser at the REPL using the `browse-api` macro or its shorthand `ba`. For example:

> `(browse-api jframe)`  
> `(ba string)`  
> `(ba element)`

In the last case you will get two document pages about `Element`; one is about `javax.xml.bind.Element` of Java SE 6 and the other is about `javax.lang.model.element.Element` of Java EE 6. Give a fully qualified class/interface name if you want to be specific and get just one page. For example:

> `(ba javax.xml.bind.Element)`

Also, when you want to use a nested class name, such as `HTML.Tag` of the `javax.swing.text.html` package, use '$' instead of '.' in the class name like `html$tag`.

Ced also knows how to call API-browser. For example, open a Ced and type the following lines:

		(import '(javax.swing.text Segment)
		        'javax.swing.text.html.HTML$Tag)

Put the caret on "`Segment`" and press F1. Likewise, put the caret on "`javax.swing.text.html.HTML$Tag`" and press F1. You will get the document pages of `Segment` and `HTML$Tag` in the browser.

### 3.1.1 Browsing Local Document

By default API-browser looks up document URL in the Java class index html file on the Oracle server ("http://download.oracle.com/javase/6/docs/api/allclasses-noframe.html"  and "http://download.oracle.com/javaee/6/api/allclasses-noframe.html"). So your machine has to be on the net.

You can download the copies of the Java SE 6 and Java EE 6 API documents and put them on your local hard disk for faster browsing. Get download the document zip files from the Software Downloads page at the Oracle web site. Extract them and move the root `docs` directories to the directory `doc/apidocs` to get the directory structure look like below:

> `doc/apidocs/J2SE/6.0/`*the-docs-directory-of-the-JavaSE6-API-documentation*  
> `doc/apidocs/JavaEE/6.0/`*the-docs-directory-of-the-JavaEE6-API-documentation*

Make sure that the following html files are accessible.

>`doc/apidocs/J2SE/6.0/docs/api/allclasses-noframe.html`  
>`doc/apidocs/JavaEE/6.0/docs/api/allclasses-noframe.html`

The server URLs and local document paths are defined in the file `src/library/slix/api_browser/apidoc_urls`.
