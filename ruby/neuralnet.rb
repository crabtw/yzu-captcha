class NeuralNet
  def initialize(io)
    @layers = eval(io.gets)
  end

  def calculate(inputs)
    @layers.reduce(inputs) do |input, layer|
      layer.map do |neuron|
        sum = 0
        input.zip(neuron) do |i, w|
          sum += i * w
        end

        1.0 / (1.0 + Math.exp(-sum))
      end
    end
  end
end
