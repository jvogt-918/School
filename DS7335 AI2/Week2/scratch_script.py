import torch
import torch.nn as nn
import matplotlib.pyplot as plt

# Generate synthetic data: y = 2x + 1 + noise
torch.manual_seed(42)
X = torch.randn(1000, 1) * 10
y = 2 * X + 1 + torch.randn(1000, 1) * 2

def train_with_batch_size(batch_size, epochs=50):
    model = nn.Linear(1, 1)
    optimizer = torch.optim.SGD(model.parameters(), lr=0.01)
    criterion = nn.MSELoss()

    losses = []
    for epoch in range(epochs):
        # Shuffle and create batches
        perm = torch.randperm(len(X))
        epoch_loss = 0
        num_batches = 0

        for i in range(0, len(X), batch_size):
            batch_idx = perm[i:i+batch_size]
            X_batch, y_batch = X[batch_idx], y[batch_idx]

            optimizer.zero_grad()
            pred = model(X_batch)
            loss = criterion(pred, y_batch)
            loss.backward()
            optimizer.step()

            epoch_loss += loss.item()
            num_batches += 1

        losses.append(epoch_loss / num_batches)

    return losses, model.weight.item(), model.bias.item()

# Compare different batch sizes
batch_sizes = [1, 32, 256, 1000]  # SGD, mini-batch, large batch, full batch
results = {}

for bs in batch_sizes:
    losses, final_w, final_b = train_with_batch_size(bs)
    results[bs] = {'losses': losses, 'w': final_w, 'b': final_b}
    print(f"Batch size {bs}: Final w={final_w:.3f}, b={final_b:.3f}")
