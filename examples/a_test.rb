require "test/unit"
require "#{File.dirname(__FILE__)}/failure.rb"

class FailingTest < Test::Unit::TestCase

  def test_recurse_and_then_fails
    assert_nothing_raised recurse(5)
  end

  def test_a_failure
    assert_equal "one", "other"
  end

end
