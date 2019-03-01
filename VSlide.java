import java.util.HashSet;
import java.util.Set;
import java.util.Collection;

public class VSlide extends Slide {
	
	private Image a;
	private Image b;

	public VSlide(Image a, Image b) {
		this.a = a;
		this.b = b;
	}

	public Collection<String> getTags() {
		Set<String> set = new HashSet<String>();

		set.addAll(a.getTags());
		set.addAll(b.getTags());

		return set;
	}

	public String toString() {
		return this.a.index + " " + this.b.index;
	}
}
