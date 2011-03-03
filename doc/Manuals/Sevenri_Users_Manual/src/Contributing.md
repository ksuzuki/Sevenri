## 6 Contributing

Please report issus on the [GitHub issue tracker](https://github.com/ksuzuki/Sevenri/issues) or the [mailing list](http://groups.google.com/group/sevenri). Your comments are welcome.

If you would like to join the Sevenri dev team, send your message to [ksuzuki](https://github.com/ksuzuki).

### 6.1 GitHub Issue Tracker

The GitHub issue tracker allows us to report issues. This is the format to use when reporting issues.

#### 6.1.1 General Rules

File and line number can be expressed like file-name:number. e.g. "`core.clj:123`"

#### 6.1.2 Title

Use this format for issue title.

> [SLIX: | *slix-name* [, *slix-name* ...]:] *short-description*

* When the issue is a non-slix issue, just write a short description.
* When the issue is a slix-wise issue, write "SLIX: " then a short description.
* When the issue is regarding a slix, write "slix-name: " then a short description.
* When the issue is regarding slixes, write "slix-name1, slix-name2,...: " then a short description.

Apply appropriate label when possible.

#### 6.1.3 Boby

Write a detailed description and reproduction steps at least. Optionally add full exception stack trace as "Log" if you think it's useful.

--- Start template ---

	# Description
	Your detailed description here.

	# Repro steps
	Write steps to reproduce the issue. For example,
	1. open slix X.
	2. Click the button B.
	3. java.io.IOException at oplix.x.core.clj:123.

	# Log
	Optionally attach the exception stack trace dump from the Log-viewer. Enclose the dump using <pre><code> and </code></pre> for readability.

--- End template ---
