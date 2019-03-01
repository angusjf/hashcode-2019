import java.util.List;
import java.util.Collection;

public class Image {
	
	public int index;
	public boolean isHorizontal;
	private Collection<String> tags;

	public Image(boolean isHorizontal, List<String> tags, int i) {
		this.isHorizontal = isHorizontal;
		this.tags = tags;
		this.index = i;
	}

	public Collection<String> getTags() {
		return tags;
	}

	public String toString() {
		return "{[" + index + "] :" + (isHorizontal ? "h" : "v") + "}";
	}
}
