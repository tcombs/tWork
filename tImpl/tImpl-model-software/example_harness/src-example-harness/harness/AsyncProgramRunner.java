package harness;

import java.io.File;
import java.io.IOException;

/**
 * Runs a given executable in a given directory.
 */
public class AsyncProgramRunner extends Thread {
  private String programPath;
  private File workingDirectory;
  private boolean shouldExit = false;
  
  private boolean shouldExecute = false; 
  
  private ProcessBuilder pb;
  
  public AsyncProgramRunner() {
    setDaemon(true);
  }
  
  void setProgramPath(final String programPath) {
    synchronized(this) {
      this.programPath = programPath;
    }
    updateProcessBuilder();
  }
  
  public String getProgramPath() {
    String ret = null;
    synchronized(this) {
      ret = new String(programPath.getBytes());
    }
    return ret;
  }
  
  void setWorkingDirectory(final File workingDirectory) {
    synchronized(this) {
      this.workingDirectory = workingDirectory;
    }
    updateProcessBuilder();
  }
  
  public File getWorkingDirectory() {
    return workingDirectory;
  }
  
  private void updateProcessBuilder() {
    if(programPath == null || workingDirectory == null) {
      pb = null;
      return;
    }
    synchronized(this.programPath) {
      pb = new ProcessBuilder(programPath);
    }
    synchronized(this.workingDirectory) {
      pb.directory(workingDirectory);
    }
  }
  
  /**
   * Requests that the executable be called once. Subsequent calls before or during
   * an async execution will be ignored.
   */
  public void requestExecute() {
    synchronized(this) {
      shouldExecute = true;
    }
  }
  
  public void exit() throws InterruptedException {
    synchronized(this) {
      shouldExit = true;
    }
    join();
  }
  
  @Override
  public void run() {
    while(!shouldExit) {
      boolean execute = false;
      synchronized(this) {
        execute = shouldExecute;
      }
      if(!execute) continue;
      
      Process p = null;
      synchronized(this) {
        try {
          p = pb.start();
        } catch (final IOException e) {
          e.printStackTrace();
        }
      }
      
      if(p == null) continue;
      
      
      
      try {
        p.waitFor();
      } catch (final InterruptedException e) {
        e.printStackTrace();
      }
      
      synchronized(this) {
        shouldExecute = false;
      }
    }
  }
}

