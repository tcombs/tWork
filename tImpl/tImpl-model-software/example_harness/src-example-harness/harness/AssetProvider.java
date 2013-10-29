package harness;

import java.io.IOException;

public interface AssetProvider {
  public boolean updateAsset();
  public String retrieveAsset() throws IOException;
}
