function f = score(x,y,z)
  f= ( (1+z^2)*(x.*y) ./ ( ((1+z^2)*x)+y) )
  
end
