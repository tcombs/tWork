package harness;

import java.io.IOException;

import javax.swing.JFrame;

public class Main {
  public static void main(final String[] args) throws IOException {
	  final int frameRate = 2; //number of display updates per second
	  final int msec = 1000 / frameRate; // msec between frame updates
    // Start up our GUI
    GUI gui = new GUI();
    gui.setVisible(true);
    gui.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    
    // Start up our HTTP server on port 8888
    HttpServer server =
      new HttpServer(8888, new AssetRefresher(gui, msec));
    server.setDaemon(true);
    server.start();
  }
}
