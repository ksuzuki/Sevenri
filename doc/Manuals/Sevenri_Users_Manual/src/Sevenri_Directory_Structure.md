## 5 Sevenri Directory Structure

This section describes the overview of the Sevenri directory structure.

### .sevenri/
Contains various files of Sevenri and slixes. These sub directories are reserved: classes/, log/, sevenri/, slix/, temp/, trash/.

>### classes/
>Contains aot-compiled slix files.

>### log/
>Contains log files.

>### sevenri/
>Contains Sevenri system files created and used at runtime. These sub directory and file are reserved: excetpion/, lock.

>>### exception/
>>Contains files the Sevenri's logging facility uses.

>>### lock
>>Sevenri creates this file at startup and uses it to prevent multiple Sevenris starting from the same Sevenri directory. Removed when Sevenri quits.

>### slix/
>Contains saved slix instance files. This file is reserved: \_startup\_.clj

>>### \_startup\_.clj
>>Sevenri uses this file to remember running slix instances at shutdown and open them at startup.

>### temp/
>A directory Sevenri and slix can use to save temporary files.

>### trash/
>Contains most recently deleted files.

### classes/
Leiningen creates this directory when downloading project dependencies. Also Sevenri lib files compiled by leiningen go here.

### doc/
Contains any "document" files. These sub directories are reserved: apidocs/, liceses/, Manuals/, Misc/.

>### apidocs/
>Contains any API documents. These sub directories are reserved: J2SE/, JavaEE/.

>>### J2SE/
>Contains local Java 2 SE documents.

>>### JavaEE/
>Contains local Java EE documents.

>### licenses/
>Contains license files.

>### Manuals/
>Contains manual files.

>### Misc/
>Contains miscellaneous files. 

### lib/
Contains files Sevenri depends.

### src/
Contains lib and resource files for Sevenri system, slix, and user. These sub directories are reserved: library/, sevenri/, slix/, resources/.

>### Sevenri.clj
>The Sevenri boot loader file

>### *.class
>Aot-compiled Sevenri boot loader files

>### library/
>Contains sharable lib files. These sub directories are reserved: slix/, user/.

>>### slix/
>>Contains slix lib files sharable between slixes.

>>### user/
>>A directory where Sevenri user can save any user lib files. This file is reserved: scratch.

>>>### scratch
>>>A scratch file Ced opens by default

>### sevenri/
>Contains Opeanr system lib files.

>### slix/
>Contains slix lib files.

>### resources/
>Contains resources files for Sevenri system and slixes. These sub directories are reserved: image/, logger/.

### temp/
Sevenri user can use this directory to save any files.

### test/
Contains the Sevenri system test files.

### tools/
Contains files used to build Sevenri.

### COPYING
The license file of Sevenri

### README.md
A Sevenri readme file

### sevenri
A bash file you can use to start Sevenri

### sevenri.app
A compiled AppleScript file you can use to start Sevenri

### project.clj
The Sevenri project configuration file for leiningen
