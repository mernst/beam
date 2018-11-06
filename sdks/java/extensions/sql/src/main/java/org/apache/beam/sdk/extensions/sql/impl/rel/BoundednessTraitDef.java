/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.beam.sdk.extensions.sql.impl.rel;

import static com.google.common.base.Preconditions.checkArgument;

import org.apache.beam.sdk.values.PCollection;
import org.apache.calcite.plan.RelOptPlanner;
import org.apache.calcite.plan.RelTraitDef;
import org.apache.calcite.plan.RelTraitSet;
import org.apache.calcite.rel.RelNode;

/** Trait definitionn for whether a relation is bounded or unbounded. */
public final class BoundednessTraitDef extends RelTraitDef<BoundednessTrait> {
  public static final BoundednessTraitDef INSTANCE = new BoundednessTraitDef();

  private BoundednessTraitDef() {}

  @Override
  public Class<BoundednessTrait> getTraitClass() {
    return BoundednessTrait.class;
  }

  @Override
  public String getSimpleName() {
    return "boundedness";
  }

  @Override
  public RelNode convert(
      RelOptPlanner planner,
      RelNode rel,
      BoundednessTrait toTrait,
      boolean allowInfiniteCostConverters) {

    // Not actually a conversion
    if (rel.getTraitSet().getTrait(this).isBounded() == toTrait.isBounded()) {
      return rel;
    }

    // Else we are converting bounded to unbounded
    checkArgument(
        (rel.getTraitSet().getTrait(this).isBounded() == PCollection.IsBounded.BOUNDED)
            && (toTrait.isBounded() == PCollection.IsBounded.UNBOUNDED),
        "Can only convert bounded to unbounded");

    // Conversion is a noop, but update the trait set
    RelTraitSet newTraitSet = rel.getTraitSet().replace(BoundednessTrait.UNBOUNDED);
    RelNode newRel = rel.copy(newTraitSet, rel.getInputs());
    return newRel;
  }

  /** We can convert bounded to unbounded but otherwise only identity conversions. */
  @Override
  public boolean canConvert(
      RelOptPlanner planner, BoundednessTrait fromTrait, BoundednessTrait toTrait) {
    return fromTrait.isBounded() == PCollection.IsBounded.BOUNDED
        || (fromTrait.isBounded() == toTrait.isBounded());
  }

  @Override
  public BoundednessTrait getDefault() {
    return null;
  }
}
