FROM ocaml/opam:ubuntu-lts

# Switch to root to perform initial setup
USER root

# Create the ComRaTT directory
RUN mkdir -p /home/opam/ComRaTT
WORKDIR /home/opam/ComRaTT

# Copy project files and directories
COPY lib ./lib
COPY bin ./bin
COPY examples ./examples
COPY ComRaTT.opam ./
COPY dune-project ./
COPY run_ratt.sh ./

# Ensure correct ownership
RUN chown -R opam:opam /home/opam/ComRaTT
RUN chmod +x run_ratt.sh

# Switch back to opam user
USER opam

# Install project dependencies
RUN opam install . --deps-only

# Install Wasmtime
RUN curl https://wasmtime.dev/install.sh -sSf | bash

# Optional: Add Wasmtime to PATH if needed
ENV PATH="/home/opam/.wasmtime/bin:${PATH}"

# Default command (can be overridden)
CMD ["/bin/bash"]
