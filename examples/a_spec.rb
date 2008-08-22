require "failure"

describe "A failing spec" do

  it "recurses 5 times and then fails" do
    lambda { recurse(5) }.should_not raise_error
  end
  
  it "this is a simple failure" do
    "this".should == "that"
  end
end
