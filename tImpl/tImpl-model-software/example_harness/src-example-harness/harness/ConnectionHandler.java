package harness;

import java.io.IOException;
import java.net.Socket;

public interface ConnectionHandler {
  public void handle(final Socket s) throws IOException; 
}
