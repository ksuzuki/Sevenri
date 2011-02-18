## 3.6 Log-viewer

oplix name: `log-viewer`

Log-viewer is a singleton oplix and the instance is called the Log-viewer. The Log-viewer displays the content of the current log file, updating it every one second.

![Log-viewer](../res/ss-log-viewer.png "Log-viewer")

Openar opens a new log file at startup. Up to four most recent log files are saved. Messages printed from the Openar's logging functions are saved to log file. Openar also captures output data written to *System/out* and *System/err* streams and saves it to log file.
