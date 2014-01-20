#OEUnit

OEUnit is a unit testing framework for OpenEdge ABL. OEUnit is intended to help write and run repeatable unit tests - similar to JUnit and other xUnit-based unit testing frameworks.

For full documentation, see **doc/index.html** in the repository.

##Screenshot
Below is a screenshot of the test results docked in Progress Developer Studio (previously OpenEdge Architect).

![Image](/doc/images/oea_example1.png?raw=true)

##Change Log
###1.4

- Merged changes to better support automation and XML reporting. Thanks to Arek Jaworski and Mark Abbott.
- Refactored merged changes to remove redundant code sections and folder structure
- Removed 'Author' from the file headers - a leftover relic from a heading template and not conducive to social coding :)
- Tested with OpenEdge 11.3

###1.3

- Fixed bug where the results window would display for a moment and then disappear.
- Fixed bug where compile error 468 is thrown when r-code already exists for a test class (Compile aborted. SAVE not specified and r-code file exists)
- Fixed bug the DYNAMIC-NEW failed to create an instance based on the class file-name alone. When r-code was also present in the directory.
- Improved documentation in line with the new Progress Developer Studio (previously named OpenEdge Architect)
- Tested with OpenEdge 11.2

###1.2

- OEUnit 1.2 requires OpenEdge 10.2b
- Changes to show the error message returned from simple RETURN ERROR "error message" statements
- Updated UI/ResultsWindow.w to show 'RUNNING' while tests are running
- Increased the default height & width of the UI/DetailsWindow.w and improved general readability
- Updated code to use the new ABSTRACT keyword introduced in OpenEdge 10.2b
- Updated Reflection/MethodInfo.cls to use the new DYNAMIC-INVOKE function introduced in OpenEdge 10.2b - removed Util/CallMethod.p
- Updated Runner/BaseRunner.cls to use the new EVENT keyword introduced in OpenEdge 10.2b
- Simplified interfaces for Util/List.cls and Util/IComparator.cls
- Added a change log to the project documentation

###1.1
- Added changes to support the OpenEdge 10.2a runtime
- Corrected the method modifiers in UI/ResultsWindowView.cls - causing compile time errors in 10.2a, but was somehow working in 10.1c ?
- Fixed bug in Remove() method of Util/List.cls - elements were being re-indexed incorrectly

###1.0
- Initial release - basic functionality to run test cases and suites.
