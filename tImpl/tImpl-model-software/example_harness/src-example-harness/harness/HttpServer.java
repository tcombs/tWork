package harness;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

public class HttpServer extends Thread {
  private boolean stop = false;
  private ServerSocket socket;
  private ConnectionHandler connHandler;
  
  public HttpServer(final int port, final ConnectionHandler connHandler) throws IOException {
    socket = new ServerSocket(port);
    this.connHandler = connHandler;
  }
  
  public InetAddress getAddress() {
    return socket.getInetAddress();
  }
  
  private void handleNext() {
    try {
      final Socket client = socket.accept();
      Thread handlerThread = new Thread(new Runnable() {
        @Override
        public void run() {
          try {
            connHandler.handle(client);
            client.close();
          } catch (final Exception e) {
            e.printStackTrace();
          }
        }
      });
      handlerThread.start();
    } catch (final IOException e) {
      e.printStackTrace();
    } 
  }
  
  public void run() {
    while(!stop) handleNext();
    stop = false;
  }
  
  public void exit() {
    stop = true;
  }
}
