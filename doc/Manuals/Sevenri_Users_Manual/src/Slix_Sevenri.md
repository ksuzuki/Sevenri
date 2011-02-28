## 3.7 Sevenri

slix name: `sevenri`

The slix `sevenri` is a singleton slix and the instance is called the Sevenri.

![The Sevenri](../res/ss-the-sevenri.png "The Sevenri")

When Sevenri starts, it opens the Sevenri always. You can't close the Sevenri. The Sevenri has two panes; the left pane lists available slixes and the right pane lists slix instance names with its window title. Double-clicking a slix name in the left pane opens the slix, and the instance name will be shown in the right pane. Selecting slix names in the left pane shows their instance names in the right pane. Selecting a slix instance name in the right pane and then double-clicking it brings the instance window to the front.

The Sevenri also shows the name "Sevenri" with the version number in the top area. The version number is consisted of major, minor, and incremental numbers. Like Clojure's versioning scheme, feature releases may increment minor and/or major, and bugfix releases will increment incremental. 

### 3.7.1 Opening The Main Lib File of Slix

You can open the main lib file of slix using the Sevenri. Hold down the META key and double-click a slix name in the left pane. The Sevenri opens the main lib file of the slix in Ced.
