package harness;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.Scanner;

public class AssetRefresher implements ConnectionHandler {
  private final static String HTTP_HEADER = 
		  "HTTP/1.1 OK\nContent-Type: text/html;\n\n";

  private AssetProvider assetProvider;
  private int msec;
  
  public AssetRefresher(final AssetProvider assetProvider,
		                final int msec) {
    this.assetProvider = assetProvider;
    this.msec = msec;
  }
  
  private String createJavascript() {
    final StringBuffer buff = new StringBuffer();
    buff.append("setInterval(function() {");
    
    // Create an HTTP request and send it to ourself
    buff.append("var x = new XMLHttpRequest();");
    buff.append("x.open(\"GET\", \"asset\", false);");
    buff.append("x.send();");
    
    // Set the <body> to our new HTML
    buff.append("document.getElementById(\"main\").innerHTML = x.responseText;");
    
    buff.append("}, ");
    buff.append(msec);
    buff.append(");");
    return buff.toString();
  }
  
  /**
   * Writes out a simple HTML page with an embedded javascript
   * program that refreshes the given asset at the given frame rate
   * 
   * @param out
   * @throws IOException
   */
  private void handleRefresherPayload(final OutputStream out)
		       throws IOException {
    out.write(HTTP_HEADER.getBytes());
    out.write("<html><body id=\"main\">".getBytes());
    out.write("<script>".getBytes());
    out.write(createJavascript().getBytes());
    out.write("</script>".getBytes());
    out.write("</body></html>".getBytes());
  }
  
  /**
   * Writes out the actual asset (that was requested by the HTML script)
   * 
   * @param out
   * @throws IOException
   */
  private void handleAssetPayload(final String url,
		                          final OutputStream out) throws IOException {
	synchronized(this) {
	    final boolean goAhead = assetProvider.updateAsset();
	    if(!goAhead) {
	      out.write("<b>Program offline</b>".getBytes());
	      return;
	    }
	    
	    out.write(assetProvider.retrieveAsset().getBytes());
	}
  }
  
  @Override
  public void handle(final Socket client) throws IOException {
    final InputStream in = client.getInputStream();
    final OutputStream out = client.getOutputStream();
    
    // The beginning of an HTTP Request looks like this:
    // GET /the/url HTTP/1.1
    final Scanner requestReader = new Scanner(in);
    requestReader.useDelimiter(" ");
    
    // Get the request type (hopefully GET)
    if(!requestReader.hasNext()) {
      requestReader.close();
      throw new IOException("Expected request type");
    }
    final String type = requestReader.next();
    if(type.compareToIgnoreCase("GET") != 0) {
      requestReader.close();
      throw new IOException("Can't process non-GET requests");
    }
    
    // Next, read the url
    if(!requestReader.hasNext()) {
      requestReader.close();
      throw new IOException("Expected url");
    }
    final String url = requestReader.next();
    
    if(url.compareToIgnoreCase("/") == 0) {
      // If the user requested the root url,
      // serve up the initial refresher payload
      handleRefresherPayload(out);
    } else if (url.compareTo("/asset") == 0) {
      // Otherwise, serve up the asset described by the URL
      handleAssetPayload(url, out);
    } else {
      requestReader.close();
      throw new IOException("Unknown URL " + url);
    }
    
    requestReader.close();
  }
}
