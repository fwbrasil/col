package io.col;

public class Test {
  
  public static class Nest {
    public Boolean b;
    public Nest(Boolean b) {
      this.b = b;
    }
  }

  public String a;
  public int b;
  public Nest c;
  

  public Test(String a, int b, Nest c) {
    super();
    this.a = a;
    this.b = b;
    this.c = c;
  }
}
