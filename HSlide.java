import java.util.Collection;

public class HSlide extends Slide {

	private Image image;

	public HSlide(Image image) {
		this.image = image;
	}

	public Collection<String> getTags() {
		return image.getTags();
	}

	public String toString() {
		return String.valueOf(image.index);
	}
}
