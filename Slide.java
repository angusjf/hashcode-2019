import java.util.Collection;

public abstract class Slide {
	
	public int getNumberOfTags() {
		return getTags().size();
	}

	public abstract Collection<String> getTags();

	private static long getNumberOfCommonTags(Slide a, Slide b) {
		return a.getTags()
			.stream()
		 	.filter(x -> b.getTags().contains(x))
			.count();
	}

	public int getScoreWith(Slide slide) {
		return (int)Math.min(
			getNumberOfCommonTags(this, slide),
			Math.min(
				slide.getNumberOfTags(),
				this.getNumberOfTags()
			)
		);
	}
}

