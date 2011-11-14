/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/29/11
 * Time: 2:44 AM
 * 
 */

import java.io.{InputStreamReader, BufferedReader, File}
import javax.swing.JFileChooser
import org.apache.commons.io.FileUtils

object Script {
  case class ScriptException(s:String) extends Throwable(s)
  implicit def string2se(s:String) = ScriptException(s)

  def dropExtension(s:String) = dropUntilChar(s.reverse, '.').reverse

  def dropUntilChar(s: String, c: Char): String = if(s.contains(c)) s.dropWhile(c != ).tail else s

  /**
  * Performs the script action on the given name. Overwrite denotes whether to overwrite files that already exist.
  * This method will return None for success (nothing went wrong), Option[Throwable] for an error
  */
  def runProcess(args:String*)(dir:File = null) = {
    val pb = new ProcessBuilder(args:_*).redirectErrorStream(true).directory(dir)

    val process = pb.start(); //runs proguard
    val inputRedirect = new Thread() {
      override def run() = try {
        val in = new BufferedReader(new InputStreamReader(process.getInputStream))
        var line = ""
        while (!Thread.currentThread.isInterrupted && {
          line = in.readLine; line != null
        }) {
          println(args(0)+": " + line)
          Thread.sleep(16);
        }
        println(args(0)+" finished!")
      } catch { case e: InterruptedException => (); case e => e.printStackTrace() }
    };
    inputRedirect.start();
    process.waitFor();
    inputRedirect.interrupt();
    process.exitValue()
  }

  def doScript(file:File, width:String, height:String, overwrite:Boolean = false):Option[Throwable] = try {
    //The source will be specified relative to Daily\, e.g. src\daily\Jul12.scala
    if(!file.exists()) return Some(file+" doesn't exist!");
    if(file.isDirectory) return Some(file+" is a directory!");
    val sketchName = dropExtension(file.getName).capitalize //this is basically to get rid of the ".scala" extension
    //e.g. Jul12.scala => alacs.12luJ => .12luJ => 12luJ => Jul12
    //I would like to specify subpackages for where my scala file lives (e.g. I'd like to place Jul12 in daily\jul\Jul12.scala)

    val codeName = dropExtension(dropUntilChar(file.toString, '\\')).replace("\\", "/") //daily\oct\Oct22 => daily/oct/Oct22

    println("Script: file is "+file+", sketchName is "+sketchName+", codeName is "+codeName)

    val sourceFile = file
    //prereq: do a check and make sure the file is ok.

    //1)
    val sketchFolder = new File("sketches\\"+sketchName.toLowerCase) //Files are specified relative to Daily\
    if(sketchFolder.exists() && !overwrite) return Some(sketchFolder+" exists!")
    sketchFolder.mkdir() //              Daily\sketches\jul12\ now exists

    println("1) Script: sketchFolder is "+sketchFolder)

    //2) (a)
    //This config file will be run from within the sketch folder, so Daily.jar is one level above it
    val configString =
"""-injars ..\\Daily.jar
-outjars """+sketchName+""".jar
-libraryjars 'C:\Program Files\Java\jdk1.6.0_23\jre\lib\rt.jar'

-keep public class """+codeName.replace("/", ".")+"""

-dontpreverify
-ignorewarnings
-keeppackagenames controlP5

-keep public class processing.core.* extends processing.core.PGraphics

#controlP5 uses various gifs in the controlP5 folder, so the class names must be preserved.
-keepnames class controlP5.*

# Keep all registerXxxx() method callbacks in all classes
-keepclassmembers class ** {
  public void pre();
  public void draw();
  public void post();
  public void keyEvent(java.awt.event.KeyEvent);
  public void mouseEvent(java.awt.event.MouseEvent);
  public void size(int, int);
  public void stop();
  public void dispose();
}

#======================KEEP SCALA STUFF=======================
-keepclassmembers class * {
    ** MODULE$;
}
"""

    val configFile = new File(sketchFolder, "config.pro")
    print("2a) Script: configFile is at "+configFile+". Writing... ")
    FileUtils.write(configFile, configString) //generate proguard configuration file
    println("Done!");

    //2) (b)
    print("2b) Script: Starting process \"proguard.bat @"+configFile+"\"... ");
    runProcess("proguard.bat", "@" + configFile)() match {
      case e if e != 0 => return Some("Proguard exited errornously with status "+e+"!");
      case _ => ()
    }
    println("Done!")

    //3
    print("3) Script: copying "+sourceFile+" into "+sketchFolder+"... ")
    FileUtils.copyFileToDirectory(sourceFile, sketchFolder);
    println("Done!")
    //4
//    val Size = """size\((\d+), (\d+)\);?""".r //trying to find size(400, 400);
//    val (width:String, height:String) = try {
//      sourceLines.map(_.trim).            //get rid of whitespace
//        filterNot(_.startsWith("//")).    //get rid of comments
//        find(_.startsWith("size(")) match {
//      case Some(string) => string match { case Size(w, h) => (w, h) }
//      } //nonmatching cases will throw matcherror and get catch-ed
//    } catch {
//      case _ => ("500", "500") //default to that
//    }

    val jarName = sketchName+".jar"
    
    val index =
"""<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <!-- charset must remain utf-8 to be handled properly by Processing -->
    <meta http-equiv="content-type" content="text/html; charset=utf-8"/>

    <title>"""+sketchName + """: Built with Processing</title>
    <LINK REL="stylesheet" HREF="../../sketch.css" TYPE="text/css"/>
  </head>

  <body>
    <div id="content">
      <div id="container">

        <!-- This version plays nicer with older browsers,
     but requires JavaScript to be enabled.
     http://java.sun.com/javase/6/docs/technotes/guides/jweb/deployment_advice.html -->
        <script type="text/javascript" src="http://www.java.com/js/deployJava.js"></script>
        <script type="text/javascript">
          var attributes ={code: '"""+codeName+"""', archive: '"""+jarName+"""', width: """+width+""", height: """+height+""", image: 'loading.gif'};
          var parameters = {"java_arguments": "-Djnlp.packEnabled=true" };
          var version = '1.5';
          deployJava.runApplet(attributes, parameters, version);
        </script>
        <noscript>
          <div>
            <!--[if !IE]> -->
            <object classid="java:"""+codeName+""".class"
                    type="application/x-java-applet"
                    archive="""+jarName+"""
                    width="""+width+""" height="""+width+"""
                    standby="Loading Processing software...">

                <param name="archive" value="""+jarName+"""/>

                <param name="mayscript" value="true"/>
                <param name="scriptable" value="true"/>

                <param name="image" value="loading.gif"/>
                <param name="boxmessage" value="Loading Processing software..."/>
                <param name="boxbgcolor" value="#FFFFFF"/>

                <param name="test_string" value="outer"/>
              <!--<![endif]-->

              <!-- For more instructions on deployment,
   or to update the CAB file listed here, see:
   http://java.sun.com/javase/6/webnotes/family-clsid.html
   http://java.sun.com/javase/6/webnotes/install/jre/autodownload.html -->
              <object classid="clsid:8AD9C840-044E-11D1-B3E9-00805F499D93"
                      codebase="http://java.sun.com/update/1.6.0/jinstall-6u20-windows-i586.cab"
                      width="""+width+""" height="""+height+"""
                      standby="Loading Processing software...">

                  <param name="code" value="""+codeName+"""/>
                  <param name="archive" value="""+jarName+"""/>

                  <param name="mayscript" value="true"/>
                  <param name="scriptable" value="true"/>

                  <param name="image" value="loading.gif"/>
                  <param name="boxmessage" value="Loading Processing software..."/>
                  <param name="boxbgcolor" value="#FFFFFF"/>

                  <param name="test_string" value="inner"/>

                <p>
                  <strong>
                    This browser does not have a Java Plug-in.
                      <br/>
                    <a href="http://www.java.com/getjava" title="Download Java Plug-in">
                      Get the latest Java Plug-in here.
                    </a>
                  </strong>
                </p>

              </object>

              <!--[if !IE]> -->
            </object>
            <!--<![endif]-->

          </div>
        </noscript>

      </div>

      <div id="columns">
        <div class="left column">
          <h4>Keyboard controls</h4>
          <pre></pre>
        </div>

        <div class="right column">
          <h4>Mouse controls</h4>
          <pre></pre>
        </div>
      </div>

      <p>Source code:<a href="""+sketchName+""".scala>"""+sketchName+"""</a></p>

      <p>Built with <a href="http://processing.org" title="Processing.org">Processing</a></p>
      <p><a href="../..">Back to sketches</a></p>
    </div>
  </body>
</html>""";

    val indexFile = new File(sketchFolder, "index.html")
    print("4) Script: indexFile is at "+indexFile+". Writing... ")
    FileUtils.write(indexFile, index)
    println("Done!")

    //5) pack200 that shit
    println("5) Script: pack200 "+jarName+".pack.gz "+jarName)
    runProcess("pack200", jarName+".pack.gz", jarName)(sketchFolder) match {
      case e if e != 0 => return Some("Pack200 returned errornously with exit code "+e+"!");
      case _ => ()
    }
    println("Done!");

    println("Script finished for "+sketchName+"!")
    None
  } catch {
    case e: Throwable => Some(e)
  }
  
  def main(args:Array[String]) {
    import javax.swing._

//    if(args.length == 0) { println("usage: Script [-obfuscate] argname0 [argname1 argname2 ...]"); sys.exit(1); }
    val path = new File("").getAbsolutePath
    if(!"C:\\Users\\hellochar\\Documents\\dev\\NetBeansIntelliJ\\Daily".equals(path)) {
      println("Script is not running from C:\\Users\\hellochar\\Documents\\dev\\NetBeansIntelliJ\\Daily!\nInstead it is at "+path+"!")
      sys.exit(1)
    }
    val file = {val jfc = new JFileChooser(new File("src\\daily").getAbsoluteFile); jfc.showOpenDialog(null) match {
      case JFileChooser.APPROVE_OPTION => new File(null.asInstanceOf[File], jfc.getSelectedFile.toString.drop(path.length + 1))
      case _ => sys.exit(2);
    } }
    val (width, height) = try {
      (JOptionPane.showInputDialog("Width?", 500).toInt,
       JOptionPane.showInputDialog("Height?", 500).toInt)
    } catch { case e => sys.exit(2) }

    val results = doScript(file, width.toString, height.toString, false)
    results match {
      case None => println("Success!");
      case Some(e) => e.printStackTrace()
    }
  }

}