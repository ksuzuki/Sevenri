## 5 Openar Directory Structure

This section describes the overview of the Openar directory structure.

### .openar/
Contains various files of Openar and oplixes. These sub directories are reserved: classes/, log/, openar/, oplix/, temp/, trash/.

>### classes/
>Contains aot-compiled oplix files.

>### log/
>Contains log files.

>### openar/
>Contains Openar system files created and used at runtime. These sub directory and file are reserved: excetpion/, lock.

>>### exception/
>>Contains files the Openar's logging facility uses.

>>### lock
>>Openar creates this file at startup and uses it to prevent multiple Openars starting from the same Openar directory. Removed when Openar quits.

>### oplix/
>Contains saved oplix instance files. This file is reserved: \_startup\_.clj

>>### \_startup\_.clj
>>Openar uses this file to remember running oplix instances at shutdown and open them at startup.

>### temp/
>A directory Openar and oplix can use to save temporary files.

>### trash/
>Contains most recently delete files.

### classes/
Contains aot-compiled Openar system lib files. This directory may not exist unless you compile Openar system lib files.

### doc/
Contains any "document" files. These sub directories are reserved: apidocs/, liceses/, Manuals/.

>### apidocs/
>Contains any API documents. These sub directories are reserved: J2SE/, JavaEE/.

>>### J2SE/
>Contains local Java 2 SE documents.

>>### JavaEE/
>Contains local Java EE documents.

>### licenses/
>Contains license files.

>### Manuals/
>Conains manual files.

### lib/
Contains files Openar depends.

### src/
Contains lib and resource files for Openar system, oplix, and user. These sub directories are reserved: library/, openar/, oplix/, resources/.

>### Openar.clj
>The Openar boot loader file

>### *.class
>Aot-compiled Openar boot loader files

>### library/
>Contains sharable lib files. These sub directories are reserved: oplix/, user/.

>>### oplix/
>>Contains oplix lib files sharable between oplixes.

>>### user/
>>A directory where Openar user can save any user lib files. This file is reserved: scratch.

>>>### scratch
>>>A scratch file Ced opens by default

>### openar/
>Contains Opeanr system lib files.

>### oplix/
>Contains oplix lib files.

>### resources/
>Contains resources files for Openar system and oplixes. These sub directories are reserved: image/, logger/.

### temp/
Openar user can use this directory to save any files.

### test/
Contains the Openar system test files.

### tools/
Contains files used to build Openar.

### COPYING
The license file of Openar

### README.md
An Openar readme file

### openar
A bash file you can use to start Openar

### openar.app
A compiled AppleScript file you can use to start Openar

### project.clj
The Openar project configuration file for leiningen
