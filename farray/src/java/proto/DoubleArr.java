package proto;

public final class DoubleArr extends FBase {
  public final double[] arr;

  public DoubleArr(double[] arr, int length) {
    super(length);
    this.arr = arr;
  }

  @Override
  public Object applyBoxed(int i) {
    return Double.valueOf(arr[i]);
  }
}
