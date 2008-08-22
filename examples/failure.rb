def recurse(i)
  n = i - 1
  if n > 0
    reflect(n)
  else
    raise(StandardError.new("Intentionally unwind"))
  end
end

def reflect(n)
  recurse(n - 1)
end
