idRunner components v. 3.3
============================

idRunner is a set of native Delphi components for Borland Delphi
versions  5, 6 & 7. 
100% Source Code.

TidCGIRunner component allows to execute CGI scripts using 
Indy TidHTTPServer component. 

idISAPIRunner component allows to run ISAPI modules using 
Indy TidHTTPServer.

Main features:

* PHP support for CGI and ISAPI
* WebSnap support
* IntraWeb support
* CGI file upload support
* Unload ISAPI module on demand
* WebServices support
* Session support


Internet Direct (Indy 8 or 9) library is required. 
You can download the latest version of Indy library from
http://www.nevrona.com/indy

This software is provided 'as-is', without any express or
implied warranty. In no event will the author be held liable
for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any
purpose, including commercial applications, and to alter it
and redistribute it freely without any  restrictions.


INSTALLATION:

1. Delphi 5.x:

Uninstall previous installed version of idRunner from Delphi 5 IDE.
Remove previously compiled idRunner package idRunnerD5.bpl from your hard disk.
If you are using Indy 8 please remove the following line
{$DEFINE INDY9} from the idRunner.inc file.

Use "File\Open..." menu item to open  package idRunnerD5.DPK.
In "Package..." window click "Compile" button to compile the package
and then click "Install" button to register idRunner components 
on the component palette. 


2. Delphi 6.x:

Uninstall previous installed version of idRunner components from Delphi 6 IDE.
Remove previously compiled idRunner package idRunnerD6.bpl from your hard disk.
If you are using Indy 8 please remove the following line
{$DEFINE INDY9} from the idRunner.inc file.

Use "File\Open..." menu item to open design-time package idRunnerD6.DPK.
In "Package..." window click "Compile" button to compile the package
and then click "Install" button to register idRunner components
on the component palette. 


3. Delphi 7.x:

Uninstall previous installed version of idRunner components from Delphi 7 IDE.
Remove previously compiled idRunner package idRunnerD7.bpl from your hard disk.

Use "File\Open..." menu item to open design-time package idRunnerD7.DPK.
In "Package..." window click "Compile" button to compile the package
and then click "Install" button to register idRunner components
on the component palette. 


Author:                                              
Serhiy Perevoznyk                                     
serge_perevoznyk@hotmail.com
http://users.chello.be/ws36637

