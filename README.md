# Wave analysis
##### For windows users, please complete the followed steps first:
###### Install Rtools for download .zip file
1. Install Rtools from https://cran.r-project.org/bin/windows/Rtools/.
2. Locate the folder that Rtools is installed. In my case it is at C:\Rtools.
3. Add C:\Rtools\bin path to the system path.
	1. Go to Control Panel >> System and Security >> System
	2. Go to Advanced System Settings
	3. Open Advanced tab
	4. Click Environmental Variables... button
	5. Select Path variable and click Edit button
	6. If there is nothing as a "Variable Value" you can simply write C:\Rtools\bin. If there is already a value, then add ;C:\Rtools\bin to the end of it.
	7). Click OK, OK...Restart R

##### Install FFmpeg to make the pesudocolored video
###### For Windows users
To get the pesudocolored video directly, please intall the ImageMagick first (http://www.imagemagick.org/script/download.php#windows).
###### For Mac users
To get the pesudocolored video directly, please intall the FFmpeg first (http://ffmpeg.org/download.html).
Here is a guide video to show how to install FFmpeg in Mac OS (https://www.youtube.com/watch?v=qu7tvO-Pmko).

## To run this App
Drop the Wave_analysis.R to your RStudio, then click the button "Run App" on the up right corner.

## Acknowledgement
MathJax file was copy form https://github.com/mathjax/MathJax, which as the local file to show the math equation.
To update this MathJax file, please go to this link https://github.com/mathjax/MathJax and download the mathjax as .zip file, then release the files to this Wave_analysis directory and replace them.
