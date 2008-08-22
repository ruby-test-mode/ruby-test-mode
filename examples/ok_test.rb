require "test/unit"

class OkTest < Test::Unit::TestCase
  def test_succeeds
    puts 0
    assert_equal "ok", :ok.to_s
  end
  def test_1
    assert_equal "ok", :ok.to_s
  end
  def test_2
    assert_equal "ok", :ok.to_s
  end
  def test_3
    puts 3
    assert_equal "ok", :ok.to_s
  end
  def test_4
    assert_equal "ok", :ok.to_s
  end
end
