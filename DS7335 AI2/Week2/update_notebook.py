import json

path = r"c:\Users\jvogt\OneDrive\Documents\School-1\DS7335 AI2\Week2\homework_02.ipynb"
with open(path, 'r', encoding='utf-8') as f:
    nb = json.load(f)

for i, cell in enumerate(nb['cells']):
    if cell['cell_type'] == 'code' and "z = (x * w + b)^2" in "".join(cell.get('source', [])):
        source = cell['source']
        for j, line in enumerate(source):
            if "What is dz/dy? What is dy/dx?" in line:
                source.insert(j+1, "# dz/dy = 2*y\n")
                source.insert(j+2, "# dy/dx = w\n")
                source.insert(j+3, "# dz/dx = dz/dy * dy/dx = 2*y * w = 2(7) * 3 = 42\n")
                source.insert(j+4, "# dz/dw = dz/dy * dy/dw = 2*y * x = 2(7) * 2 = 28\n")
                break

    if cell['cell_type'] == 'markdown':
        source_text = "".join(cell.get('source', []))
        if "**Your Answer**:" in source_text:
            prev_cell_text = "".join(nb['cells'][i-1].get('source', []))
            if "Question 1" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\nWe must compute gradients backward from Layer 3 to Layer 1 because the chain rule dictates that the derivative of a composite function relies on the derivatives of its outer functions. The mathematical principle is the chain rule ($dz/dx = dz/dy \\times dy/dx$). If we tried to compute Layer1's gradient first, we wouldn't have the required gradient from Layer2 needed to complete the calculation, leading to highly inefficient forward-mode differentiation."]
            elif "Question 2" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\n1. $dz/dy = 2y = 2(x \\cdot w + b)$.\n2. $dy/dw = x$.\n3. $dz/dw = dz/dy \\times dy/dw = 2y \\cdot x$. With $x=2, w=3, b=1, y=7$, so $dz/dw = 2(7) \\cdot 2 = 28$.\n4. This exactly matches PyTorch's automatic gradient output of 28.0."]
            elif "Question 3" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\nIt is called \"backpropagation\" because the error (loss) gradient is propagated backward through the network from the output layer to the input layer. The chain rule's associative property allows us to compute gradients by starting at the end (loss) and reusing intermediate gradients for earlier layers. This backward direction avoids redundant calculations, making it exponentially more efficient than computing gradients going forward."]
            elif "Question 4" in prev_cell_text:
                cell['source'] = ["**Your Answer**: B\n\n**Explanation**: The chain rule involves multiplying the local gradients of each layer. If the gradients of activation functions (like Sigmoid or Tanh) are less than 1, multiplying many of these small fractions together causes the overall gradient to shrink exponentially as it propagates back to earlier layers, leading to the vanishing gradient problem."]
            elif "Question 5" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\n1. A gradient magnitude of 0.001 indicates a very flat region in the loss landscape or that the model is near a local/global minimum.\n2. A magnitude of 100.0 indicates a very steep slope, where the loss changes rapidly.\n3. A large gradient (100.0) might require decreasing the learning rate to prevent taking too large a step and overshooting the minimum. Conversely, a small gradient (0.001) might benefit from a slightly larger learning rate to speed up convergence in flat regions."]
            elif "Question 6" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\n1. The `batch_size=1000` (full batch) had much smoother loss curves because the gradient is an exact average over the whole dataset.\n2. The `batch_size=1` (Stochastic Gradient Descent) made more weight updates per epoch (1000 updates vs 1 update).\n3. `batch_size=1` likely explored the solution space better. The frequent, noisy updates introduce stochasticity, which acts as implicit regularization and helps the optimizer \"bounce out\" of local minima or saddle points."]
            elif "Question 7" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\nThe noise from small batches introduces randomness into the optimization path. This stochastic behavior helps the model escape sharp local minima or saddle points where a smooth, deterministic full-batch gradient might get stuck. By \"bouncing around,\" the optimizer is more likely to find wider, more generalizable flat minima."]
            elif "Question 8" in prev_cell_text:
                cell['source'] = ["**Your Answer**: B\n\n**Explanation**: A learning rate that is too large causes the optimizer to take steps that are too big. This causes it to overshoot the minimum, leading to wild oscillations across the loss landscape valley, and can even cause the loss to continuously increase (diverge) instead of converging."]
            elif "Question 9" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\nBackpropagation uses the chain rule to efficiently calculate the gradients of the loss with respect to every weight in the network, essentially determining the direction and steepness of the error surface. Gradient descent then uses these computed gradients to update the weights by taking a step in the opposite direction of the gradient (downhill). Together, backpropagation provides the \"map\" (the slopes), and gradient descent takes the \"steps\" to iteratively minimize the loss and enable the model to learn."]
            elif "Question 10" in prev_cell_text:
                cell['source'] = ["**Your Answer**:\n\n1. Automatic differentiation systematically applies the chain rule to compute exact gradients in a single backward pass, regardless of network depth.\n2. Without it, training modern models with billions of parameters would be impossible. Manually deriving and coding gradient formulas for every parameter is intractable, and numerical differentiation (tweaking each parameter one by one) would take millions of times longer, making large-scale deep learning computationally unfeasible."]

with open(path, 'w', encoding='utf-8') as f:
    json.dump(nb, f, indent=2)
print("Notebook updated successfully.")
