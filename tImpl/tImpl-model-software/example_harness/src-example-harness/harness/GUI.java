package harness;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;

public class GUI extends JFrame implements AssetProvider {
  private static final long serialVersionUID = 2468802632497148995L;
  
  private JTextArea inputEditor = new JTextArea(); 
  
  private JPanel bottomBar = new JPanel();
  private JButton setExecutable = new JButton("Set Executable");
  private JButton start = new JButton("Start");
  private JButton stop = new JButton("Stop");

  private String currentExecutable = "";
  private String cachedHtmlOutput = "";
  private String cachedInput = "";
  
  private boolean started = false;
  
  private AsyncProgramRunner runner = new AsyncProgramRunner();
  
  public GUI() {
    setTitle("Example Harness vA");
    
    getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    add(inputEditor);
    add(bottomBar);
    
    setExecutable.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(final ActionEvent e) {
        setExecutablePrompt();
      }
    });
    
    start.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(final ActionEvent e) {
        started = true;
        updateState();
        try {
          writeFile(cachedInput, inputEditor.getText());
        } catch (final IOException e1) {
          e1.printStackTrace();
        }
      }
    });
    
    stop.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(final ActionEvent e) {
        started = false;
        updateState();
      }
    });
    
    bottomBar.setLayout(new FlowLayout(FlowLayout.LEFT));
    bottomBar.add(setExecutable);
    bottomBar.add(start);
    bottomBar.add(stop);
    
    setMinimumSize(new Dimension(640, 480));
    
    updateState();
    
    runner.start();
  }
  
  private void updateState() {
    final boolean goodExecutable = !currentExecutable.isEmpty();
    setExecutable.setEnabled(!started);
    start.setEnabled(goodExecutable && !started);
    stop.setEnabled(goodExecutable && started);
    inputEditor.setEnabled(goodExecutable && !started);
    
    if(!goodExecutable) return;
    try {
      findInputFile();
      inputEditor.setText(readFile(cachedInput));
    } catch (final IOException e) {
      e.printStackTrace();
      inputEditor.setEnabled(false);
    }
  }
  
  private void setExecutablePrompt() {
    final JFileChooser fc = new JFileChooser();
    fc.setFileFilter(new javax.swing.filechooser.FileFilter() {
      @Override
      public boolean accept(final File f) {
        return f.isDirectory() || f.canExecute();
      }

      @Override
      public String getDescription() {
        return "Executables";
      }
    });
    
    if(fc.showOpenDialog(this) != JFileChooser.APPROVE_OPTION) return;
    final File f = fc.getSelectedFile();
    currentExecutable = f.getAbsolutePath();
    
    runner.setProgramPath(currentExecutable);
    runner.setWorkingDirectory(executableDir());

    cachedHtmlOutput = "";
    cachedInput = "";
    updateState();
  }

  /**
   * This is called by the HTTP server when it needs a refreshed asset.
   * We simply call the executable synchronously.
   */
  @Override
  public boolean updateAsset() {
    if(currentExecutable.isEmpty() || !started) return false;
    
    runner.requestExecute();
    return true;
  }
  
  private File executableDir() {
    final File executableFile = new File(currentExecutable);
    return executableFile.getParentFile();
  }
  
  /**
   * We don't know the name of the html file the program is going to
   * create, but we do know it will probably be in the same working
   * directory. We use the first html file we find in the working
   * directory.
   */
  private void findHtmlOutput() throws IOException {
    if(!cachedHtmlOutput.isEmpty()) return;

    final File[] files = executableDir().listFiles(new FileFilter() {
      @Override
      public boolean accept(final File f) {
        return f.getName().endsWith(".htm") | f.getName().endsWith(".html");
      }
    });
    
    if(files.length == 0) throw new IOException("Couldn't find html file to serve");
    cachedHtmlOutput = files[0].getAbsolutePath();
  }
  
  /**
   * We don't know the name of the input file the program is going to
   * read, but we do know it will probably be in the same working
   * directory. We use the first .txt file we find in the working
   * directory.
   */
  private void findInputFile() throws IOException {
    final File[] files = executableDir().listFiles(new FileFilter() {
      @Override
      public boolean accept(final File f) {
        return f.getName().endsWith(".txt") |
        	   f.getName().startsWith("input");
      }
    });
    if(files.length == 0)
      throw new IOException("Couldn't find input file for reading");
    cachedInput = files[0].getAbsolutePath();
  }
  
  private static String readFile(final String fileName) throws IOException {
    final FileInputStream input = new FileInputStream(fileName);
    final StringBuilder s = new StringBuilder();
    final byte[] buffer = new byte[1024];
    while(input.available() > 0) {
      final int read = input.read(buffer);
      s.append(new String(buffer, 0, read));
    }
    input.close();
    return s.toString();
  }
  
  private static void writeFile(final String fileName,
		  final String contents) throws IOException {
    final FileOutputStream output = new FileOutputStream(fileName);
    output.write(contents.getBytes());
    output.close();
  }

  @Override
  public String retrieveAsset() throws IOException {
    try {
      findHtmlOutput();
    } catch(final IOException e) {
      return "";
    }
    
    inputEditor.setText(readFile(cachedInput));
    
    // Return the entire file's contents
    return readFile(cachedHtmlOutput);
  }
  
}
